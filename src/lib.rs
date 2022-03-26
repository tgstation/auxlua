use auxtools::{hook, runtime, shutdown, DMResult, List, Proc, Runtime};
use lua::{
    AuxluaError, GlobalWrapper, DATUM_CALL_PROC_WRAPPER, GLOBAL_CALL_PROC_WRAPPER,
    LUA_THREAD_START, SET_VAR_WRAPPER, DMValue, MluaValue,
};
use mlua::{FromLua, Function, Lua, MetaMethod, MultiValue, Table, Thread, ThreadStatus, ToLua};
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::time::Instant;

pub mod lua;

type LuaModValue = lua::Value;

thread_local! {
    /// A hashmap of all created states, used for accessing individual states
    /// from DM
    pub static STATES: RefCell<HashMap<String, Lua>> = RefCell::new(HashMap::new());

    /// A CPU usage limit in milliseconds for each run of lua code
    pub static EXECUTION_LIMIT: RefCell<u128> = RefCell::new(100);

    /// The path string of a DM proc to call first when calling `require` from lua
    pub static REQUIRE_WRAPPER: RefCell<Option<String>> = RefCell::new(None);
}

/// This function is guaranteed to return the first border of a table, unlike the default
/// \# operator, which can return any border depending on factors such as the order
/// of table initializations.
fn first_border<'lua>(_: &'lua Lua, this: Table<'lua>) -> mlua::Result<MluaValue<'lua>> {
    for i in 0..i32::MAX {
        if !this.contains_key(i + 1)? {
            return Ok(MluaValue::Integer(i));
        }
    }
    Ok(MluaValue::Integer(i32::MAX))
}

fn wrap_require(lua: &Lua, name: String) -> mlua::Result<MluaValue> {
    let wrapper_result: Option<String> = REQUIRE_WRAPPER.with(|wrapper| {
        let wrapper_proc = Proc::find((wrapper.borrow().as_ref())?)?;
        let result = wrapper_proc
            .call(&[&DMValue::from_string(name.clone()).unwrap()])
            .ok()?;
        result.as_string().ok()
    });
    if let Some(loader) = wrapper_result {
        let chunk = lua.load(&loader).set_environment(lua.globals())?;
        let evaluated_loader = chunk.eval()?;
        if let MluaValue::Function(f) = evaluated_loader {
            f.call(name)
        } else {
            Ok(evaluated_loader)
        }
    } else {
        let require: Function = lua.globals().raw_get("__require")?;
        require.call(name)
    }
}

/// Applies auxlua-specific data structures to the lua state
fn apply_state_vars(state: &Lua, id: String) -> DMResult<()> {
    let globals = state.globals();

    // Create and populate the dm table, which provides hooks into BYOND

    let dm_table = state
        .create_table()
        .map_err(|e| specific_runtime!("{}", e))?;

    // `world`, a wrapper for dm's `world`

    let world = state
        .create_userdata(GlobalWrapper::new(DMValue::world()))
        .map_err(|e| specific_runtime!("{}", e))?;
    dm_table
        .raw_set("world", world)
        .map_err(|e| specific_runtime!("{}", e))?;

    // `global_vars`, a wrapper for dm's `global`

    let dm_globals = state
        .create_userdata(GlobalWrapper::new(DMValue::globals()))
        .map_err(|e| specific_runtime!("{}", e))?;
    dm_table
        .raw_set("global_vars", dm_globals)
        .map_err(|e| specific_runtime!("{}", e))?;

    // `global_proc`, a function that calls `/proc/[proc]`

    let global_proc = state
        .create_function(lua::global_proc_call)
        .map_err(|e| specific_runtime!("{}", e))?;
    dm_table
        .raw_set("global_proc", global_proc)
        .map_err(|e| specific_runtime!("{}", e))?;

    // `sleep`, a function that yields a thread to be resumed
    // as soon as possible

    let sleep: Function = state
        .load(
            r#"function()
        __sleep_flag = true
        coroutine.yield()
        __sleep_flag = nil
        return
    end"#,
        )
        .eval()
        .map_err(|e| specific_runtime!("{}", e))?;
    dm_table
        .raw_set("sleep", sleep)
        .map_err(|e| specific_runtime!("{}", e))?;

    // `state_id`, the key of the state in the global hashmap

    dm_table
        .raw_set("state_id", id)
        .map_err(|e| specific_runtime!("{}", e))?;

    dm_table.set_readonly(true);

    globals
        .raw_set("dm", dm_table)
        .map_err(|e| specific_runtime!("{}", e))?;

    // Create the task info table, used to store information about
    // the currently running tasks

    let task_info = state
        .create_table()
        .map_err(|e| specific_runtime!("{}", e))?;
    task_info.set_readonly(true);
    globals
        .raw_set("__task_info", task_info)
        .map_err(|e| specific_runtime!("{}", e))?;

    // Create the sleep queue, used to store sleeping tasks

    let sleep_queue = state
        .create_table()
        .map_err(|e| specific_runtime!("{}", e))?;
    sleep_queue.set_readonly(true);
    globals
        .raw_set("__sleep_queue", sleep_queue)
        .map_err(|e| specific_runtime!("{}", e))?;

    // Create the yield table, used to store tasks that
    // have yielded and are awaiting resumes with arguments

    let yield_table = state
        .create_table()
        .map_err(|e| specific_runtime!("{}", e))?;
    let yield_metatable = state.create_table().map_err(|e| specific_runtime!(e))?;
    let yield_len = state
        .create_function(first_border)
        .map_err(|e| specific_runtime!(e))?;
    yield_metatable
        .raw_set("__len", yield_len)
        .map_err(|e| specific_runtime!(e))?;
    yield_metatable.set_readonly(true);
    yield_table.set_metatable(Some(yield_metatable));
    yield_table.set_readonly(true);
    globals
        .raw_set("__yield_table", yield_table)
        .map_err(|e| specific_runtime!("{}", e))?;

    let original_require: Function = globals
        .raw_get("require")
        .map_err(|e| specific_runtime!("{}", e))?;
    globals
        .raw_set("__require", original_require)
        .map_err(|e| specific_runtime!("{}", e))?;
    let require = state
        .create_function(wrap_require)
        .map_err(|e| specific_runtime!("{}", e))?;
    globals
        .raw_set("_require", require)
        .map_err(|e| specific_runtime!("{}", e))?;
    let require_wrapper = state
        .create_function::<_, MluaValue, _>(|lua: &Lua, name: String| {
            let require: Function = lua.globals().raw_get("_require")?;
            lua.load_from_function(&name, require)
        })
        .map_err(|e| specific_runtime!("{}", e))?;
    globals
        .raw_set("require", require_wrapper)
        .map_err(|e| specific_runtime!("{}", e))?;

    let environment = state
        .create_table()
        .map_err(|e| specific_runtime!("{}", e))?;
    let env_metatable = state
        .create_table()
        .map_err(|e| specific_runtime!("{}", e))?;
    env_metatable
        .set(MetaMethod::Index.name(), globals.clone())
        .map_err(|e| specific_runtime!("{}", e))?;
    env_metatable.set_readonly(true);
    environment.set_metatable(Some(env_metatable));
    globals
        .raw_set("env", environment)
        .map_err(|e| specific_runtime!("{}", e))?;
    state
        .set_interrupt(exhaustion_check)
        .map_err(|e| specific_runtime!("{}", e))?;
    state.sandbox().map_err(|e| specific_runtime!("{}", e))?;
    Ok(())
}

/// A CPU limiter for lua states.
fn exhaustion_check(_: &Lua) -> mlua::Result<()> {
    LUA_THREAD_START.with(|start| {
        EXECUTION_LIMIT.with(|limit| {
            if start.borrow().elapsed().as_millis() > *limit.borrow() {
                Err(external!(
                    "execution limit reached - call dm.sleep or coroutine.yield before this point"
                ))
            } else {
                Ok(())
            }
        })
    })
}

#[hook("/proc/__lua_set_set_var_wrapper")]
fn set_set_var_wrapper(wrapper: DMValue) {
    wrapper.as_string().and_then(|wrapper_string| {
        SET_VAR_WRAPPER.with(|wrapper| {
            *wrapper.borrow_mut() = Some(wrapper_string);
            Ok(DMValue::null())
        })
    })
}

#[hook("/proc/__lua_set_datum_proc_call_wrapper")]
fn set_datum_proc_call_wrapper(wrapper: DMValue) {
    wrapper.as_string().and_then(|wrapper_string| {
        DATUM_CALL_PROC_WRAPPER.with(|wrapper| {
            *wrapper.borrow_mut() = Some(wrapper_string);
            Ok(DMValue::null())
        })
    })
}

#[hook("/proc/__lua_set_global_proc_call_wrapper")]
fn set_global_proc_call_wrapper(wrapper: DMValue) {
    wrapper.as_string().and_then(|wrapper_string| {
        GLOBAL_CALL_PROC_WRAPPER.with(|wrapper| {
            *wrapper.borrow_mut() = Some(wrapper_string);
            Ok(DMValue::null())
        })
    })
}

#[hook("/proc/__lua_set_require_wrapper")]
fn set_require_wrapper(wrapper: DMValue) {
    wrapper.as_string().and_then(|wrapper_string| {
        REQUIRE_WRAPPER.with(|wrapper| {
            *wrapper.borrow_mut() = Some(wrapper_string);
            Ok(DMValue::null())
        })
    })
}

#[hook("/proc/__lua_set_execution_limit")]
fn set_execution_limit(limit: DMValue) {
    limit.as_number().and_then(|limit_num| {
        EXECUTION_LIMIT.with(|execution_limit| {
            *execution_limit.borrow_mut() = (limit_num as u128).to_owned();
            Ok(DMValue::null())
        })
    })
}

#[hook("/proc/__lua_new_state")]
fn new_state() {
    let new_state = Lua::new_with(mlua::StdLib::ALL_SAFE, mlua::LuaOptions::default())
        .map_err(|e| specific_runtime!("{}", e))?;
    let state_hash: String = format!("{:p}", &new_state);
    apply_state_vars(&new_state, state_hash.clone())?;
    STATES.with(|states| {
        states
            .borrow_mut()
            .insert(state_hash.clone(), new_state);
        Ok(DMValue::from_string(state_hash).unwrap())
    })
}

fn get_task_table_info<'lua, S>(
    lua: &'lua Lua,
    coroutine: Thread<'lua>,
    name: S,
) -> mlua::Result<Table<'lua>>
where
    S: Into<String>,
{
    let task_info_table: Table = lua.globals().raw_get("__task_info")?;
    task_info_table.raw_get(coroutine.clone()).or_else(|_| {
        lua.create_table()
            .and_then(|table| {
                table.raw_set("name", name.into())?;
                table.set_readonly(true);
                task_info_table.raw_set(coroutine.clone(), table)
            })
            .and(task_info_table.raw_get(coroutine.clone()))
    })
}

fn handle_coroutine_return<T>(lua: &Lua, coroutine: Thread, name: T) -> mlua::Result<i32>
where
    T: Into<String>,
{
    let globals = lua.globals();
    let task_info_table: Table = lua.globals().raw_get("__task_info")?;
    task_info_table.set_readonly(false);
    if coroutine.status() == ThreadStatus::Resumable {
        let sleep_flag: MluaValue = globals
            .raw_get("__sleep_flag")
            .map_err(|e| specific_external!(e))?;
        if sleep_flag == mlua::Nil {
            let yield_table: Table = globals
                .raw_get("__yield_table")
                .map_err(|e| specific_external!(e))?;
            yield_table.set_readonly(false);
            let first_border = yield_table.len().map_err(|e| specific_external!(e))?;
            yield_table
                .raw_set(first_border + 1, coroutine.clone())
                .map_err(|e| specific_external!(e))?;
            let task_info = get_task_table_info(lua, coroutine, name)?;
            task_info.set_readonly(false);
            task_info.raw_set("status", "yield")?;
            task_info.raw_set("index", first_border + 1)?;
            task_info.set_readonly(true);
            yield_table.set_readonly(true);
            return Ok(first_border + 1);
        } else {
            let sleep_queue: Table = globals
                .raw_get("__sleep_queue")
                .map_err(|e| specific_external!(e))?;
            sleep_queue.set_readonly(false);
            let queue_len = sleep_queue.raw_len();
            sleep_queue
                .raw_set(queue_len + 1, coroutine.clone())
                .map_err(|e| specific_external!(e))?;
            let task_info = get_task_table_info(lua, coroutine, name)?;
            task_info.set_readonly(false);
            task_info.raw_set("status", "sleep")?;
            task_info.raw_set("index", queue_len + 1)?;
            task_info.set_readonly(true);
            sleep_queue.set_readonly(true);
            return Ok(0);
        }
    } else {
        task_info_table.raw_remove(coroutine)?;
    }
    task_info_table.set_readonly(true);
    Ok(0)
}

#[hook("/proc/__lua_load")]
fn load(state: DMValue, script: DMValue, name: DMValue) {
    let key = state.as_string()?;
    let script_text = script.as_string()?;
    let name = name.as_string().ok();
    STATES.with(|states| {
        if let Some(lua_state) = states.borrow_mut().get_mut(&key) {
            lua_state
                .globals()
                .raw_get("env")
                .and_then(|env: Table| lua_state.load(&script_text).set_environment(env))
                .and_then(|chunk| match name.clone() {
                    Some(name_string) => chunk.set_name(&name_string),
                    None => Ok(chunk),
                })
                .and_then(|chunk| chunk.into_function())
                .and_then(|chunk_func| lua_state.create_thread(chunk_func))
                .and_then(|coroutine| {
                    LUA_THREAD_START.with(|start| *start.borrow_mut() = Instant::now());
                    let ret: mlua::Result<MultiValue> = coroutine.resume(mlua::Nil);
                    let status = coroutine.status();
                    let task_name = name.unwrap_or_else(|| String::from("input"));
                    let yield_index =
                        handle_coroutine_return(lua_state, coroutine, task_name.clone())?;
                    Ok((status, ret, yield_index, task_name))
                })
                .and_then(|res| coroutine_result(res, lua_state))
                .or_else(|e| Ok(DMValue::from_string(format!("{}", e)).unwrap()))
        } else {
            Err(specific_runtime!("No lua state at {}", key))
        }
    })
}

#[hook("/proc/__lua_get_globals")]
fn get_globals(state: DMValue) {
    let key = state.as_string()?;
    STATES.with(|states| {
        if let Some(lua_state) = states.borrow().get(&key) {
            let ret: List = List::new();
            let env: Table = lua_state
                .globals()
                .raw_get("env")
                .map_err(|e| specific_runtime!("{}", e))?;
            for pair in env.pairs().filter_map(Result::ok) {
                let (table_key, value): (MluaValue, MluaValue) = pair;
                let key_string = DMValue::from_string(
                    String::from_lua(table_key, lua_state)
                        .map_err(|e| specific_runtime!("{}", e))?,
                )
                .map_err(|e| specific_runtime!(e.message))?;
                let val_clone = value.clone();
                if let Ok(dm_value) = LuaModValue::from_lua(val_clone, lua_state) {
                    let arg = DMValue::try_from(dm_value)?;
                    ret.set(key_string, arg)?;
                } else {
                    let typename = value.type_name();
                    let mut typename_string = String::from(typename);
                    // prefix the typename to specify that this is an abstraction of a
                    // type not convertible to DM
                    typename_string.insert_str(0, "__lua_");
                    let value_typename = DMValue::from_string(typename_string)
                        .map_err(|e| specific_runtime!(e.message))?;
                    ret.set(key_string, value_typename)?;
                }
            }
            Ok(DMValue::from(ret))
        } else {
            Err(specific_runtime!("No lua state at {}", key))
        }
    })
}

#[hook("/proc/__lua_get_tasks")]
fn get_tasks(state: DMValue) {
    let key = state.as_string()?;
    STATES.with(|states| {
        if let Some(lua_state) = states.borrow().get(&key) {
            let ret: List = List::new();
            let env: Table = lua_state
                .globals()
                .raw_get("__task_info")
                .map_err(|e| specific_runtime!("{}", e))?;
            for pair in env.pairs() {
                if pair.is_ok() {
                    let (_, value): (Thread, Table) =
                        pair.map_err(|e| specific_runtime!("{}", e))?;
                    let info_list = List::new();
                    for info_pair in value.pairs() {
                        let (info_key, info_value): (DMValue, DMValue) = info_pair
                            .map_err(|e| specific_runtime!("{}", e))
                            .and_then(|(k, v): (LuaModValue, LuaModValue)| {
                                Ok((DMValue::try_from(k)?, DMValue::try_from(v)?))
                            })?;
                        info_list.set(info_key, info_value)?;
                    }
                    ret.append(info_list);
                }
            }
            Ok(DMValue::from(ret))
        } else {
            Err(specific_runtime!("No lua state at {}", key))
        }
    })
}

#[hook("/proc/__lua_call")]
fn call(state: DMValue, function: DMValue, arguments: DMValue) {
    let key = state
        .as_string()
        .map_err(|e| specific_runtime!(e.message))?;
    let function_path = function
        .as_list()
        .or_else(|_| {
            function.as_string().map(|_| {
                let new_path = List::new();
                new_path.append(function.clone());
                new_path
            })
        })
        .map_err(|_| specific_runtime!("function must be a string or a list of table keys"))?;
    let mapped_path = (1..=function_path.len())
        .map(|i| {
            let value = function_path.get(i)?;
            LuaModValue::try_from(&value).map_err(|e| specific_runtime!(e.message))
        })
        .collect::<DMResult<Vec<LuaModValue>>>()?;
    STATES.with(|states| {
        if let Some(lua_state) = states.borrow().get(&key) {
            let arguments_list = arguments.as_list().unwrap_or_else(|_| List::new());
            let func_args: Vec<MluaValue> = (1..=arguments_list.len())
                .map(|i| {
                    LuaModValue::try_from(&arguments_list.get(i)?)?
                        .to_lua(lua_state)
                        .map_err(|e| specific_runtime!(e))
                })
                .collect::<Result<Vec<MluaValue>, Runtime>>()?;
            let function_name: String =
                mapped_path
                    .iter()
                    .try_fold(String::new(), |mut acc, elem| {
                        if !acc.is_empty() {
                            acc += ".";
                        }
                        let value = elem
                            .clone()
                            .to_lua(lua_state)
                            .map_err(|e| specific_runtime!(e))?;
                        let token = lua_state.coerce_string(value.clone()).map_or_else(
                            |_| Ok(String::from(value.type_name())),
                            |string_opt| {
                                if let Some(string) = string_opt {
                                    Ok(String::from(
                                        string.to_str().map_err(|e| specific_runtime!(e))?,
                                    ))
                                } else {
                                    Ok(String::from("nil"))
                                }
                            },
                        )?;
                        acc += token.as_str();
                        Ok(acc)
                    })?;
            lua_state
                .globals()
                .raw_get("env")
                .and_then(|env: Table| {
                    let mut might_be_table_or_func: MluaValue = MluaValue::Table(env);
                    for token in mapped_path.clone() {
                        if let MluaValue::Table(t) = might_be_table_or_func {
                            might_be_table_or_func =
                                t.get(token).map_err(|e| specific_external!(e))?;
                        } else {
                            break;
                        }
                    }
                    Function::from_lua(might_be_table_or_func, lua_state)
                        .map_err(|e| specific_external!(e))
                })
                .and_then(|function| lua_state.create_thread(function))
                .and_then(|coroutine| {
                    LUA_THREAD_START.with(|start| *start.borrow_mut() = Instant::now());
                    let ret: mlua::Result<MultiValue> =
                        coroutine.resume(MultiValue::from_vec(func_args));
                    let status = coroutine.status();
                    let yield_index =
                        handle_coroutine_return(lua_state, coroutine, function_name.clone())?;
                    Ok((status, ret, yield_index, function_name))
                })
                .and_then(|res| coroutine_result(res, lua_state))
                .or_else(|e| Ok(DMValue::from_string(format!("{}", e)).unwrap()))
        } else {
            Err(specific_runtime!("No lua state at {}", key))
        }
    })
}

#[hook("/proc/__lua_resume")]
fn resume(state: DMValue, index: DMValue, arguments: DMValue) {
    let key = state
        .as_string()
        .map_err(|e| specific_runtime!(e.message))?;
    STATES.with(|states| {
        if let Some(lua_state) = states.borrow().get(&key) {
            let table_index = index
                .as_number()
                .map_err(|e| specific_runtime!(e.message))? as i32;
            let arguments_list = arguments.as_list().unwrap_or_else(|_| List::new());
            let resume_args: Vec<MluaValue> = (1..=arguments_list.len())
                .map(|i| {
                    LuaModValue::try_from(&arguments_list.get(i)?)?
                        .to_lua(lua_state)
                        .map_err(|e| specific_runtime!(e))
                })
                .collect::<Result<Vec<MluaValue>, Runtime>>()?;
            let globals = lua_state.globals();
            globals
                .raw_get("__yield_table")
                .and_then(|yield_table: Table| {
                    let table_item: MluaValue = yield_table.raw_get(table_index)?;
                    yield_table.raw_set(table_index, mlua::Nil)?;
                    Ok(Thread::from_lua(table_item, lua_state).ok())
                })
                .and_then(|possible_thread: Option<Thread>| match possible_thread {
                    None => Ok(DMValue::null()),
                    Some(coroutine) => {
                        LUA_THREAD_START.with(|start| *start.borrow_mut() = Instant::now());
                        let result = coroutine.resume(MultiValue::from_vec(resume_args));
                        let status = coroutine.status();
                        let task_info: Table = globals.raw_get("__task_info")?;
                        let this_tasks_info: Table = task_info.raw_get(coroutine.clone())?;
                        let name: String = this_tasks_info.raw_get("name")?;
                        let yield_index =
                            handle_coroutine_return(lua_state, coroutine, name.clone())?;
                        coroutine_result((status, result, yield_index, name), lua_state)
                    }
                })
                .or_else(|e| Ok(DMValue::from_string(format!("{}", e)).unwrap()))
        } else {
            Err(specific_runtime!("No lua state at {}", key))
        }
    })
}

#[hook("/proc/__lua_awaken")]
fn awaken(state: DMValue) {
    let key = state
        .as_string()
        .map_err(|e| specific_runtime!(e.message))?;
    STATES.with(|states| {
        if let Some(lua_state) = states.borrow().get(&key) {
            let globals = lua_state.globals();
            globals
                .raw_get("__sleep_queue")
                .and_then(|yield_table: Table| {
                    let table_item: MluaValue = yield_table.raw_get(1)?;
                    yield_table.raw_remove(1)?;
                    Ok(Thread::from_lua(table_item, lua_state).ok())
                })
                .and_then(|possible_thread: Option<Thread>| {
                    match possible_thread {
                        None => Ok(DMValue::null()),
                        Some(coroutine) => {
                            LUA_THREAD_START.with(|start| *start.borrow_mut() = Instant::now());
                            let result = coroutine.resume(mlua::Nil);
                            let status = coroutine.status();
                            let task_info: Table = globals.raw_get("__task_info")?;
                            let this_tasks_info: Table = task_info.raw_get(coroutine.clone())?;
                            let name: String = this_tasks_info.raw_get("name")?;
                            let yield_index =
                                handle_coroutine_return(lua_state, coroutine, name.clone())?;
                            coroutine_result((status, result, yield_index, name), lua_state)
                        }
                    }
                })
                .or_else(|e| Ok(DMValue::from_string(format!("{}", e)).unwrap()))
        } else {
            Err(specific_runtime!("No lua state at {}", key))
        }
    })
}

fn coroutine_result(
    (status, ret, yield_index, name): (ThreadStatus, mlua::Result<MultiValue>, i32, String),
    state: &Lua,
) -> mlua::Result<DMValue> {
    let return_list = &List::new();
    let status_string = match status {
        ThreadStatus::Resumable => "yielded",
        ThreadStatus::Unresumable => "finished",
        ThreadStatus::Error => "errored",
    };
    return_list
        .set(
            DMValue::from_string("name").unwrap(),
            DMValue::from_string(name).unwrap(),
        )
        .unwrap();
    if yield_index != 0 {
        return_list
            .set(
                DMValue::from_string("yield_index").unwrap(),
                yield_index as f32,
            )
            .unwrap()
    }
    return_list
        .set(
            DMValue::from_string("status").unwrap(),
            DMValue::from_string(status_string).unwrap(),
        )
        .unwrap();
    ret.and_then(|values| {
        let return_value_list = List::new();
        let sleep_flag: MluaValue = state.globals().raw_get("__sleep_flag")?;
        if sleep_flag != mlua::Nil && status == ThreadStatus::Resumable {
            return_list
                .set(
                    DMValue::from_string("status").unwrap(),
                    DMValue::from_string("sleeping").unwrap(),
                )
                .unwrap();
            return Ok(DMValue::from(return_list));
        }
        values
            .into_iter()
            .try_for_each(|value| {
                let converted_value = DMValue::try_from(LuaModValue::from_lua(value, state)?)
                    .map_err(|e: Runtime| external!(e.message))?;
                return_value_list.append(converted_value);
                Ok(())
            })
            .map(|_| {
                return_list
                    .set(DMValue::from_string("param").unwrap(), return_value_list)
                    .unwrap();
                DMValue::from(return_list)
            })
    })
    .or_else(|e| {
        if status_string != "errored" {
            return_list
                .set(
                    DMValue::from_string("status").unwrap(),
                    DMValue::from_string("bad return").unwrap(),
                )
                .unwrap();
        }
        return_list
            .set(
                DMValue::from_string("param").unwrap(),
                DMValue::from_string(format!("{}", e)).unwrap(),
            )
            .unwrap();
        Ok(DMValue::from(return_list))
    })
}

#[hook("/proc/__lua_kill_task")]
fn kill_task(state: DMValue, task_info: DMValue) {
    let key = state
        .as_string()
        .map_err(|e| specific_runtime!(e.message))?;
    STATES.with(|states| {
        if let Some(lua_state) = states.borrow().get(&key) {
            let globals = lua_state.globals();
            let global_task_info_table: Table = globals
                .raw_get("__task_info")
                .map_err(|e| specific_runtime!(e))?;
            let task_info_list = task_info
                .as_list()
                .map_err(|e| specific_runtime!(e.message))?;
            let task_info_value = lua::tablify_list(task_info_list)?
                .to_lua(lua_state)
                .map_err(|e| specific_runtime!(e))?;
            let target_task_info_table =
                Table::from_lua(task_info_value, lua_state).map_err(|e| specific_runtime!(e))?;
            match global_task_info_table
                .clone()
                .pairs::<Thread, Table>()
                .collect::<mlua::Result<Vec<(Thread, Table)>>>()
                .map_err(|e: mlua::Error| specific_runtime!(e))?
                .iter()
                .find(|(_, info)| {
                    for info_pair in info.clone().pairs::<MluaValue, MluaValue>().flatten() {
                        let (key, value) = info_pair;
                        if let Ok(info_value) =
                            target_task_info_table.raw_get::<_, MluaValue>(key)
                        {
                            return info_value == value;
                        }
                    }
                    false
                }) {
                Some((coroutine, info)) => {
                    let index: i32 = info.raw_get("index").map_err(|e| specific_runtime!(e))?;
                    match info
                        .raw_get::<_, String>("status")
                        .map_err(|e| specific_runtime!(e))?
                        .as_str()
                    {
                        "sleep" => {
                            let sleep_queue: Table = globals
                                .raw_get("__sleep_queue")
                                .map_err(|e| specific_runtime!(e))?;
                            sleep_queue
                                .raw_remove(index)
                                .map_err(|e| specific_runtime!(e))?;
                            Ok(())
                        }
                        "yield" => {
                            let yield_table: Table = globals
                                .raw_get("__yield_table")
                                .map_err(|e| specific_runtime!(e))?;
                            yield_table
                                .raw_set(index, mlua::Nil)
                                .map_err(|e| specific_runtime!(e))?;
                            Ok(())
                        }
                        _ => Err(specific_runtime!("invalid task status")),
                    }?;
                    global_task_info_table
                        .raw_remove(coroutine.clone())
                        .map_err(|e| specific_runtime!(e))?;
                    Ok(DMValue::null())
                }
                None => Ok(DMValue::null()),
            }
        } else {
            Err(specific_runtime!("No lua state at {}", key))
        }
    })
}

#[shutdown]
fn shutdown() {
    STATES.with(|states| states.borrow_mut().clear())
}
