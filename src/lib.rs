// Don't use nightly. Nightly is unstable and should not be used for production.
// Use the once_cell crate.
#![feature(once_cell)]

// You use this in one place, just write it by hand.
#![feature(try_find)]

// Please alias Value here to something, it's causing a lot of confusion between auxtools values and Lua values
use auxtools::{hook, runtime, shutdown, Value, List, Proc, DMResult, Runtime};
use std::time::Instant;
use std::convert::{TryFrom};
use std::cell::RefCell;
use std::collections::HashMap;
use mlua::{Lua, Table, Thread, MetaMethod, FromLua, ToLua, Function, Debug, HookTriggers, ThreadStatus, MultiValue};
use lua::{GlobalWrapper, AuxluaError, LUA_THREAD_START, GLOBAL_CALL_PROC_WRAPPER, DATUM_CALL_PROC_WRAPPER, SET_VAR_WRAPPER};

// Remove this
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

pub mod lua;

thread_local!{
    // It's not obvious what any of these variables are for.
    pub static CONTEXTS: RefCell<HashMap<String, Lua>> = RefCell::new(HashMap::new());
    pub static EXECUTION_LIMIT: RefCell<u128> = RefCell::new(100);
    pub static REQUIRE_WRAPPER: RefCell<Option<String>> = RefCell::new(None);
}

// All these functions need to be documented on what they're doing.
// It is requiring I put a lot of thought in on figuring it out.
fn first_border<'lua>(_: &'lua Lua, this: Table<'lua>) -> mlua::Result<mlua::Value<'lua>> {
    // Are you sure you need this explicit type? It should be infered from mlua::Value::Integer
    let i: i64 = 0;
    while i < i64::MAX {
        let value: mlua::Value = this.raw_get(i+1)?;
        // Use contains instead
        if value == mlua::Nil {
            // Return directly
            break;
        }
    }
    Ok(mlua::Value::Integer(i))
}

fn wrap_require<'lua>(lua: &'lua Lua, name: String) -> mlua::Result<mlua::Value> {
    let wrapper_result: Option<String> = REQUIRE_WRAPPER.with(|wrapper| {
        let wrapper_proc = Proc::find((wrapper.borrow().as_ref())?)?;
        let result = wrapper_proc.call(&[&Value::from_string(name.clone()).unwrap()]).ok()?;
        // Drop Some and the ?, `ok()` returns an Option on its own.
        Some(result.as_string().ok()?)
    });
    if let Some(loader) = wrapper_result {
        let chunk = lua.load(&loader).set_environment(lua.globals())?;
        let evaluated_loader = chunk.eval()?;
        if let mlua::Value::Function(f) = evaluated_loader {
            f.call(name)
        } else {
            Ok(evaluated_loader)
        }
    } else {
        let require: Function = lua.globals().raw_get("__require")?;
        require.call(name)
    }
}

// Most of this seems strange, I would like to see documentation on all of this so I know what is useless.
fn apply_context_vars(context: &Lua, id: String) -> DMResult<()> {
    // Need way more whitespace here, these are all clumping together.
    let globals = context.globals();
    let dm_table = context.create_table().map_err(|e| specific_runtime!("{}", e))?;
    let world = context.create_userdata(GlobalWrapper::new(Value::world())).map_err(|e| specific_runtime!("{}", e))?;
    dm_table.raw_set("world", world).map_err(|e| specific_runtime!("{}", e))?;
    let dm_globals = context.create_userdata(GlobalWrapper::new(Value::globals())).map_err(|e| specific_runtime!("{}", e))?;
    dm_table.raw_set("global_vars", dm_globals).map_err(|e| specific_runtime!("{}", e))?;
    let global_proc = context.create_function(lua::global_proc_call).map_err(|e| specific_runtime!("{}", e))?;
    dm_table.raw_set("global_proc", global_proc).map_err(|e| specific_runtime!("{}", e))?;
    let sleep: Function = context.load(r#"function()
        sleep_flag = true
        coroutine.yield()
        sleep_flag = nil
        return
    end"#).eval().map_err(|e| specific_runtime!("{}", e))?;
    dm_table.raw_set("sleep", sleep).map_err(|e| specific_runtime!("{}", e))?;
    dm_table.raw_set("context_id", id).map_err(|e| specific_runtime!("{}", e))?;
    globals.raw_set("dm", dm_table).map_err(|e| specific_runtime!("{}", e))?;
    let task_info = context.create_table().map_err(|e| specific_runtime!("{}", e))?;
    globals.raw_set("__task_info", task_info).map_err(|e| specific_runtime!("{}", e))?;
    let sleep_queue = context.create_table().map_err(|e| specific_runtime!("{}", e))?;
    globals.raw_set("__sleep_queue", sleep_queue).map_err(|e| specific_runtime!("{}", e))?;
    let yield_table = context.create_table().map_err(|e| specific_runtime!("{}", e))?;
    let yield_metatable = context.create_table().map_err(|e| specific_runtime!(e))?;
    let yield_len = context.create_function(first_border).map_err(|e| specific_runtime!(e))?;
    yield_metatable.raw_set("__len", yield_len).map_err(|e| specific_runtime!(e))?;
    yield_table.set_metatable(Some(yield_metatable));
    globals.raw_set("__yield_table", yield_table).map_err(|e| specific_runtime!("{}", e))?;
    let original_require: Function = globals.raw_get("require").map_err(|e| specific_runtime!("{}", e))?;
    // __require and _require seems very dumb. I figure one of them is internal, but it's still dumb.
    globals.raw_set("__require", original_require).map_err(|e| specific_runtime!("{}", e))?;
    let require = context.create_function(wrap_require).map_err(|e| specific_runtime!("{}", e))?;
    globals.raw_set("_require", require).map_err(|e| specific_runtime!("{}", e))?;
    let require_wrapper = context.create_function::<_, mlua::Value, _>(|lua: &Lua, name: String| {
        let require: Function = lua.globals().raw_get("_require")?;
        lua.load_from_function(&name, require)
    }).map_err(|e| specific_runtime!("{}", e))?;
    globals.raw_set("require", require_wrapper).map_err(|e| specific_runtime!("{}", e))?;
    let environment = context.create_table().map_err(|e| specific_runtime!("{}", e))?;
    let env_metatable = context.create_table().map_err(|e| specific_runtime!("{}", e))?;
    env_metatable.set(MetaMethod::Index.name(), globals.clone()).map_err(|e| specific_runtime!("{}", e))?;
    environment.set_metatable(Some(env_metatable));
    globals.raw_set("env", environment).map_err(|e| specific_runtime!("{}", e))?;
    context.set_hook(HookTriggers::every_nth_instruction(1), exhaustion_check).map_err(|e| specific_runtime!("{}", e))?;
    Ok(())
}

fn exhaustion_check (_: &Lua, debug: Debug) -> mlua::Result<()> {
    LUA_THREAD_START.with(|start| EXECUTION_LIMIT.with(|limit| {
        if start.borrow().elapsed().as_millis() > *limit.borrow() {
            Err(external!("{}:{}: execution limit reached - call dm.sleep or coroutine.yield before this point",
                std::str::from_utf8(debug.names().name.unwrap_or_default()).unwrap_or("anonymous function"),
                debug.curr_line()))
        } else {
            Ok(())
        }
    }))
}

#[hook("/proc/__lua_set_set_var_wrapper")]
fn set_set_var_wrapper(wrapper: Value) {
    wrapper.as_string().and_then(|wrapper_string| SET_VAR_WRAPPER.with(|wrapper| {
        *wrapper.borrow_mut() = Some(wrapper_string);
        Ok(Value::null())
    }))
}

#[hook("/proc/__lua_set_datum_proc_call_wrapper")]
fn set_datum_proc_call_wrapper(wrapper: Value) {
    wrapper.as_string().and_then(|wrapper_string| DATUM_CALL_PROC_WRAPPER.with(|wrapper| {
        *wrapper.borrow_mut() = Some(wrapper_string);
        Ok(Value::null())
    }))
}

#[hook("/proc/__lua_set_global_proc_call_wrapper")]
fn set_global_proc_call_wrapper(wrapper: Value) {
    wrapper.as_string().and_then(|wrapper_string| GLOBAL_CALL_PROC_WRAPPER.with(|wrapper| {
        *wrapper.borrow_mut() = Some(wrapper_string);
        Ok(Value::null())
    }))
}

#[hook("/proc/__lua_set_require_wrapper")]
fn set_require_wrapper(wrapper: Value) {
    wrapper.as_string().and_then(|wrapper_string| REQUIRE_WRAPPER.with(|wrapper| {
        *wrapper.borrow_mut() = Some(wrapper_string);
        Ok(Value::null())
    }))
}

#[hook("/proc/__lua_set_execution_limit")]
fn set_execution_limit(limit: Value) {
    limit.as_number().and_then(|limit_num| EXECUTION_LIMIT.with(|execution_limit| {
        *execution_limit.borrow_mut() = (limit_num as u128).to_owned();
        Ok(Value::null())
    }))
}

#[hook("/proc/__lua_new_context")]
fn new_context() {
    let new_context = Lua::new_with(mlua::StdLib::ALL_SAFE, mlua::LuaOptions::default())
    .map_err(|e| specific_runtime!("{}", e))?;
    // Now that you'll be on edition 2021, you can use interpolation.
    // format!("{new_context:p}") if I remember right.
    let context_hash: String = format!("{:p}", &new_context);
    apply_context_vars(&new_context, context_hash.clone())?;
    CONTEXTS.with(|contexts| {
        contexts.borrow_mut().insert(context_hash.clone(), new_context);
        Ok(Value::from_string(context_hash).unwrap())
    })
}

// Don't just use T as the generic here, use something like S to indicate it's a string.
// But also, I don't see why this is using a generic in the first place.
// Everything seems to be putzing around with strings, why not just &str?
// If not, the bound should be AsRef<str> instead
fn get_task_table_info<'lua, T>(lua: &'lua Lua, coroutine: Thread<'lua>, name: T) -> mlua::Result<Table<'lua>> 
where T: Into<String> {
    let task_info_table: Table = lua.globals().raw_get("__task_info")?;
    task_info_table.raw_get(coroutine.clone())
    .or_else(|_| 
        lua.create_table()
        .and_then(|table| {
            table.raw_set("name", name.into())?;
            task_info_table.raw_set(coroutine.clone(), table)
        })
        .and(task_info_table.raw_get(coroutine.clone())))
}

fn handle_coroutine_return<T>(lua: &Lua, coroutine: Thread, name: T) -> mlua::Result<i64> 
where T: Into<String> {
    let globals = lua.globals();
    let task_info_table: Table = lua.globals().raw_get("__task_info")?;
    if coroutine.status() == ThreadStatus::Resumable {
        let sleep_flag: mlua::Value = globals.raw_get("sleep_flag")
        .map_err(|e| specific_external!(e))?;
        // Inverse this--if-not-else is hard to read, especially when this is so long.
        if sleep_flag != mlua::Nil {
            let sleep_queue: Table = globals.raw_get("__sleep_queue")
            .map_err(|e| specific_external!(e))?;
            let queue_len = sleep_queue.raw_len();
            sleep_queue.raw_set(queue_len+1, coroutine.clone())
            .map_err(|e| specific_external!(e))?;
            let task_info = get_task_table_info(lua, coroutine, name)?;
            task_info.raw_set("status", "sleep")?;
            task_info.raw_set("index", queue_len+1)?;
            return Ok(0)
        } else {
            let yield_table: Table = globals.raw_get("__yield_table")
            .map_err(|e| specific_external!(e))?;
            let first_border = yield_table.len().map_err(|e| specific_external!(e))?;
            yield_table.raw_set(first_border+1, coroutine.clone()).map_err(|e| specific_external!(e))?;
            let task_info = get_task_table_info(lua, coroutine, name)?;
            task_info.raw_set("status", "yield")?;
            task_info.raw_set("index", first_border+1)?;
            return Ok(first_border+1)
        }
    } else {
        task_info_table.raw_remove(coroutine)?;
    }
    Ok(0)
}

#[hook("/proc/__lua_load")]
fn load(context: Value, script: Value, name: Value) {
    let key = context.as_string()?;
    let script_text = script.as_string()?;
    let name = name.as_string().ok();
    CONTEXTS.with(|contexts| {
        if let Some(lua_context) = contexts.borrow_mut().get_mut(&key) {
            // Just extract this into its own function and `?`, all these and_thens are tough to get through
            lua_context.globals().raw_get("env")
            .and_then(|env: Table| lua_context.load(&script_text).set_environment(env))
            // We are owning name (from name.as_string().ok()), then cloning it, then giving it back as a reference?
            // Why all these intermediate steps?
            .and_then(|chunk| match name.clone() {
                Some(name_string) => chunk.set_name(&name_string),
                None => Ok(chunk)
            })
            .and_then(|chunk| chunk.into_function())
            .and_then(|chunk_func| lua_context.create_thread(chunk_func))
            .and_then(|coroutine| {
                LUA_THREAD_START.with(|start| *start.borrow_mut() = Instant::now());
                let ret: mlua::Result<MultiValue> = coroutine.resume(mlua::Nil);
                let status = coroutine.status();
                let task_name = name.unwrap_or(String::from("input"));
                let yield_index = handle_coroutine_return(lua_context, coroutine, task_name.clone())?;
                Ok((status, ret, yield_index, task_name))
            }).and_then(|res| coroutine_result(res, lua_context))
            .or_else(|e| Ok(Value::from_string(format!("{}", e)).unwrap()))
        } else {
            Err(specific_runtime!("No lua context at {}", key))
        }
    })
}

#[hook("/proc/__lua_get_globals")]
fn get_globals(context: Value) {
    let key = context.as_string()?;
    CONTEXTS.with(|contexts| {
        if let Some(lua_context) = contexts.borrow().get(&key) {
            let ret: List = List::new();
            let env: Table = lua_context.globals().raw_get("env").map_err(|e| specific_runtime!("{}", e))?;
            for pair in env.pairs() {
                // Early-continue, or `.filter(Pair::is_ok)` in the initial loop.
                if pair.is_ok() {
                    let (table_key, value): (mlua::Value, mlua::Value) = pair.unwrap();
                    let key_string = Value::from_string(String::from_lua(table_key, lua_context)
                    .map_err(|e| specific_runtime!("{}", e))?).map_err(|e| specific_runtime!(e.message))?;
                    let val_clone = value.clone();
                    if let Ok(dm_value) = lua::Value::from_lua(val_clone, lua_context) {
                        let arg: auxtools::Value = Value::try_from(dm_value)?;
                        ret.set(key_string, arg)?;
                    } else {
                        let typename = value.type_name();
                        let mut typename_string = String::from(typename);
                        // What is this and what is its naming scheme
                        typename_string.insert_str(0, "__lua_");
                        let value_typename = Value::from_string(typename_string).map_err(|e| specific_runtime!(e.message))?;
                        ret.set(key_string, value_typename)?;
                    }
                }
            }
            Ok(Value::from(ret))
        } else {
            // Early-return
            Err(specific_runtime!("No lua context at {}", key))
        }
    })
}

#[hook("/proc/__lua_get_tasks")]
fn get_tasks(context: Value) {
    let key = context.as_string()?;
    CONTEXTS.with(|contexts| {
        if let Some(lua_context) = contexts.borrow().get(&key) {
            let ret: List = List::new();
            let env: Table = lua_context.globals().raw_get("__task_info").map_err(|e| specific_runtime!("{}", e))?;
            for pair in env.pairs() {
                if pair.is_ok() {
                    let (_, value): (Thread, Table) = pair.map_err(|e| specific_runtime!("{}", e))?;
                    let info_list = List::new();
                    for info_pair in value.pairs() {
                        let (info_key, info_value): (Value, Value) = info_pair.map_err(|e| specific_runtime!("{}", e))
                        .and_then(|(k, v): (lua::Value, lua::Value)| Ok((Value::try_from(k)?, Value::try_from(v)?)))?;
                        info_list.set(info_key, info_value)?;
                    }
                    ret.append(info_list);
                }
            }
            Ok(Value::from(ret))
        } else {
            // Early-return, same with everything else
            Err(specific_runtime!("No lua context at {}", key))
        }
    })
}

#[hook("/proc/__lua_call")]
fn call(context: Value, function: Value, arguments: Value) {
    let key = context.as_string().map_err(|e| specific_runtime!(e.message))?;
    let function_path = function.as_list()
        .or(function.as_string().and_then(|_| {
            let new_path = List::new();
            new_path.append(function.clone());
            Ok(new_path)
        }))
            // Clippy will probably yell at you about this.
            // You're performing these calls every time.
            // You need to use `or_else` or just simpler procedural code.
            .or(Err(specific_runtime!("function must be a string or a list of table keys")))?;
    let mapped_path = (1..=function_path.len()).map(|i| {
        let value = function_path.get(i)?;
        lua::Value::try_from(&value).map_err(|e| specific_runtime!(e.message))
    }).collect::<DMResult<Vec<lua::Value>>>()?;
    CONTEXTS.with(|contexts| {
        // This is a great use case for cargo fmt, because I can't read this at all.
        if let Some(lua_context) = contexts.borrow().get(&key) {
            let arguments_list = arguments.as_list().unwrap_or(List::new());
            let func_args: Vec<mlua::Value> = (1..=arguments_list.len())
            .map(|i| lua::Value::try_from(&arguments_list.get(i)?)?.to_lua(lua_context)
                .map_err(|e| specific_runtime!(e)))
            .collect::<Result<Vec<mlua::Value>, Runtime>>()?;
            let function_name: String = mapped_path.iter()
            // If possible, type at the variables/returns instead, rather than at the call itself.
            .try_fold::<String, _, DMResult<String>>(String::new(), |mut acc, elem| {
                if acc.len() > 0 {
                    acc += ".";
                }
                let value = elem.clone().to_lua(lua_context).map_err(|e| specific_runtime!(e))?;
                let token = lua_context.coerce_string(value.clone())
                .map_or_else(|_| Ok(String::from(value.type_name())), |string_opt| {
                    if let Some(string) = string_opt {
                        Ok(String::from(string.to_str().map_err(|e| specific_runtime!(e))?))
                    } else {
                        Ok(String::from("nil"))
                    }
                })?;
                acc += token.as_str();
                Ok(acc)
            })?;
            lua_context.globals().raw_get("env")
            .and_then(|env: Table| {
                let mut might_be_table_or_func: mlua::Value = mlua::Value::Table(env);
                for token in mapped_path.clone() {
                    if let mlua::Value::Table(t) = might_be_table_or_func {
                        might_be_table_or_func = t.get(token).map_err(|e| specific_external!(e))?;
                    } else {
                        break
                    }
                };
                Function::from_lua(might_be_table_or_func, lua_context).map_err(|e| specific_external!(e))
            })
            .and_then(|function| lua_context.create_thread(function))
            .and_then(|coroutine| {
                LUA_THREAD_START.with(|start| *start.borrow_mut() = Instant::now());
                let ret: mlua::Result<MultiValue> = coroutine.resume(MultiValue::from_vec(func_args));
                let status = coroutine.status();
                let yield_index = handle_coroutine_return(lua_context, coroutine, function_name.clone())?;
                Ok((status, ret, yield_index, function_name))
            }).and_then(|res| coroutine_result(res, lua_context))
            .or_else(|e| Ok(Value::from_string(format!("{}", e)).unwrap()))
        } else {
            Err(specific_runtime!("No lua context at {}", key))
        }
    })
}

#[hook("/proc/__lua_resume")]
fn resume(context: Value, index: Value, arguments: Value) {
    let key = context.as_string().map_err(|e| specific_runtime!(e.message))?;
    CONTEXTS.with(|contexts| {
        if let Some(lua_context) = contexts.borrow().get(&key) {
            let table_index = index.as_number().map_err(|e| specific_runtime!(e.message))? as i64;
            let arguments_list = arguments.as_list().unwrap_or(List::new());
            let resume_args: Vec<mlua::Value> = (1..=arguments_list.len())
            .map(|i| lua::Value::try_from(&arguments_list.get(i)?)?.to_lua(lua_context)
                .map_err(|e| specific_runtime!(e)))
            .collect::<Result<Vec<mlua::Value>, Runtime>>()?;
            let globals = lua_context.globals();
            globals.raw_get("__yield_table")
            .and_then(|yield_table: Table| {
                let table_item: mlua::Value = yield_table.raw_get(table_index)?;
                yield_table.raw_set(table_index, mlua::Nil)?;
                Ok(Thread::from_lua(table_item, lua_context).ok())
            })
            .and_then(|opt: Option<Thread>| {
                if let Some(coroutine) = opt {
                    LUA_THREAD_START.with(|start| *start.borrow_mut() = Instant::now());
                    let result = coroutine.resume(MultiValue::from_vec(resume_args));
                    let status = coroutine.status();
                    let task_info: Table = globals.raw_get("__task_info")?;
                    let this_tasks_info: Table = task_info.raw_get(coroutine.clone())?;
                    let name: String = this_tasks_info.raw_get("name")?;
                    let yield_index = handle_coroutine_return(lua_context, coroutine, name.clone())?;
                    coroutine_result((status, result, yield_index, name), lua_context)
                } else {
                    Ok(Value::null())
                }
            })
            .or_else(|e| Ok(Value::from_string(format!("{}", e)).unwrap()))
        } else {
            Err(specific_runtime!("No lua context at {}", key))
        }
    })
}

#[hook("/proc/__lua_awaken")]
fn awaken(context: Value) {
    let key = context.as_string().map_err(|e| specific_runtime!(e.message))?;
    CONTEXTS.with(|contexts| {
        if let Some(lua_context) = contexts.borrow().get(&key) {
            let globals = lua_context.globals();
            globals.raw_get("__sleep_queue")
            .and_then(|yield_table: Table| {
                let table_item: mlua::Value = yield_table.raw_get(1)?;
                yield_table.raw_remove(1)?;
                Ok(Thread::from_lua(table_item, lua_context).ok())
            })
            // This name isn't helpful.
            // The important part is not that it's an option, it's that it could be a thread!
            .and_then(|opt: Option<Thread>| {
                // Everything like this needs to be an early return.
                if let Some(coroutine) = opt {
                    LUA_THREAD_START.with(|start| *start.borrow_mut() = Instant::now());
                    let result = coroutine.resume(mlua::Nil);
                    let status = coroutine.status();
                    let task_info: Table = globals.raw_get("__task_info")?;
                    let this_tasks_info: Table = task_info.raw_get(coroutine.clone())?;
                    let name: String = this_tasks_info.raw_get("name")?;
                    let yield_index = handle_coroutine_return(lua_context, coroutine, name.clone())?;
                    coroutine_result((status, result, yield_index, name), lua_context)
                } else {
                    Ok(Value::null())
                }
            })
            .or_else(|e| Ok(Value::from_string(format!("{}", e)).unwrap()))
        } else {
            Err(specific_runtime!("No lua context at {}", key))
        }
    })
}

fn coroutine_result(res: (ThreadStatus, mlua::Result<MultiValue>, i64, String), context: &Lua) -> mlua::Result<Value> {
    // Do `fn coroutine_result((status, ret, yield_index, name): (types here))` instead.
    let (status, ret, yield_index, name) = res;
    let return_list = &List::new();
    // You definitely don't need the explicit type here
    let status_string: &str = match status {
        ThreadStatus::Resumable => "yielded",
        ThreadStatus::Unresumable => "finished",
        ThreadStatus::Error => "errored"
    };
    return_list.set(Value::from_string("name").unwrap(), Value::from_string(name).unwrap()).unwrap();
    if yield_index != 0 {
        return_list.set(Value::from_string("yield_index").unwrap(), yield_index as f32).unwrap()
    }
    return_list.set(Value::from_string("status").unwrap(), Value::from_string(status_string).unwrap()).unwrap();
    ret.and_then(|values| {
        let return_value_list = List::new();
        let sleep_flag: mlua::Value = context.globals().raw_get("sleep_flag")?;
        if sleep_flag != mlua::Nil && status == ThreadStatus::Resumable {
            return_list.set(Value::from_string("status").unwrap(), Value::from_string("sleeping").unwrap()).unwrap();
            return Ok(Value::from(return_list))
        }
        values.into_iter().try_for_each(|value| {
            let converted_value = Value::try_from(lua::Value::from_lua(value, context)?)
            .map_err(|e: Runtime| external!(e.message))?;
            return_value_list.append(converted_value);
            Ok(())
        }).and_then(|_| {
            return_list.set(Value::from_string("param").unwrap(), return_value_list).unwrap();
            Ok(Value::from(return_list))
        })
    }).or_else(|e| {
        if status_string != "errored" {
            return_list.set(Value::from_string("status").unwrap(), Value::from_string("bad return").unwrap()).unwrap();
        }
        return_list.set(Value::from_string("param").unwrap(), Value::from_string(format!("{}", e)).unwrap()).unwrap();
        Ok(Value::from(return_list))
    })
}

#[hook("/proc/__lua_kill_task")]
fn kill_task(context: Value, task_info: Value) {
    let key = context.as_string().map_err(|e| specific_runtime!(e.message))?;
    CONTEXTS.with(|contexts| {
        if let Some(lua_context) = contexts.borrow().get(&key) {
            let globals = lua_context.globals();
            let global_task_info_table: Table = globals.raw_get("__task_info").map_err(|e| specific_runtime!(e))?;
            let task_info_list = task_info.as_list().map_err(|e| specific_runtime!(e.message))?;
            let task_info_value = lua::tablify_list(task_info_list)?.to_lua(lua_context).map_err(|e| specific_runtime!(e))?;
            let target_task_info_table = Table::from_lua(task_info_value, lua_context).map_err(|e| specific_runtime!(e))?;
            match global_task_info_table.clone().pairs()
            .try_find(|pair: &mlua::Result<(Thread, Table)>| {
                let (_, info) = pair.as_ref().map_err(|e| e.clone())?;
                // Why clone?
                for info_pair in info.clone().pairs::<mlua::Value, mlua::Value>() {
                    let (key, value) = info_pair?;
                    if value != target_task_info_table.raw_get(key)? {
                        return Ok(false)
                    }
                    return Ok(true)
                }
                return Ok(false)
            }).map_err(|e: mlua::Error| specific_runtime!(e))? {
                Some(result) => result.map_err(|e| specific_runtime!(e)).and_then(|(coroutine, info)| {
                    let index: i64 = info.raw_get("index").map_err(|e| specific_runtime!(e))?;
                    match info.raw_get::<_, String>("status").map_err(|e| specific_runtime!(e))? {
                        // Match on the as_str() instead
                        x if x.as_str() == "sleep" => {
                            let sleep_queue: Table = globals.raw_get("__sleep_queue")
                            .map_err(|e| specific_runtime!(e))?;
                            sleep_queue.raw_remove(index)
                            .map_err(|e| specific_runtime!(e))?;
                            Ok(())
                        },
                        x if x.as_str() == "yield" => {
                            let yield_table: Table = globals.raw_get("__yield_table")
                            .map_err(|e| specific_runtime!(e))?;
                            yield_table.raw_set(index, mlua::Nil)
                            .map_err(|e| specific_runtime!(e))?;
                            Ok(())
                        },
                        _ => Err(specific_runtime!("invalid task status"))
                    }?;
                    global_task_info_table.raw_remove(coroutine)
                    .map_err(|e| specific_runtime!(e))?;
                    Ok(Value::null())
                }),
                None => Ok(Value::null())
            }
        } else {
            Err(specific_runtime!("No lua context at {}", key))
        }
    })
}

#[shutdown]
fn shutdown() {
    CONTEXTS.with(|contexts| contexts.borrow_mut().clear())
}