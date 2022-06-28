use auxtools::{hook, runtime, shutdown, DMResult, List, Runtime};
use lua::{
    AuxluaError, DMValue, GlobalWrapper, MluaValue, DATUM_CALL_PROC_WRAPPER,
    GLOBAL_CALL_PROC_WRAPPER, LUA_THREAD_START, SET_VAR_WRAPPER,
};
use mlua::{FromLua, Function, Lua, MultiValue, Table, Thread, ThreadStatus, ToLua, VmState};
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering::Relaxed;
use std::time::Instant;

pub mod lua;

type LuaModValue = lua::Value;
type LuaResult<T> = mlua::Result<T>;

macro_rules! err_as_string {
    ($expr:expr) => {
        match $expr {
            Ok(ok) => ok,
            Err(err) => {
                return DMValue::from_string(format!("{},{}: {}", std::file!(), std::line!(), err))
            }
        }
    };
}

thread_local! {
    /// A hashmap of all created states, used for accessing individual states
    /// from DM
    pub static STATES: RefCell<HashMap<String, Lua>> = RefCell::new(HashMap::new());

    /// A CPU usage limit in milliseconds for each run of lua code
    pub static EXECUTION_LIMIT: RefCell<u128> = RefCell::new(100);
}

/// This function is guaranteed to return the first border of a table, unlike the default
/// \# operator, which can return any border depending on factors such as the order
/// of table initializations.
fn first_border<'lua>(_: &'lua Lua, this: Table<'lua>) -> LuaResult<MluaValue<'lua>> {
    for i in 0..i32::MAX {
        if !this.contains_key(i + 1)? {
            return Ok(MluaValue::Integer(i));
        }
    }
    Ok(MluaValue::Integer(i32::MAX))
}

/// Gets the sleep flag
fn get_sleep_flag(lua: &Lua) -> mlua::Result<MluaValue> {
    let actual_globals: Table = lua.globals().get("_G")?;
    actual_globals.raw_get("__sleep_flag")
}

/// Sets the sleep flag
fn set_sleep_flag<'lua>(lua: &'lua Lua, enabled: MluaValue<'lua>) -> mlua::Result<()> {
    let actual_globals: Table = lua.globals().get("_G")?;
    actual_globals.set_readonly(false);
    actual_globals.raw_set("__sleep_flag", enabled)?;
    actual_globals.set_readonly(true);
    Ok(())
}

/// Sets the lua variable `dm.usr` to a weak reference to the passed in DM value
fn set_usr(lua: &Lua, usr: &DMValue) -> mlua::Result<()> {
    let actual_globals: Table = lua.globals().get("_G")?;
    let dm_table: Table = actual_globals.raw_get("dm")?;
    dm_table.set_readonly(false);
    let wrapped_usr = LuaModValue::try_from(usr).map_err(|e| external!(e.message))?;
    dm_table.raw_set("usr", wrapped_usr)?;
    dm_table.set_readonly(true);
    Ok(())
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

    // `state_id`, the key of the state in the global hashmap
    dm_table
        .raw_set("state_id", id)
        .map_err(|e| specific_runtime!("{}", e))?;

    globals
        .raw_set("dm", dm_table)
        .map_err(|e| specific_runtime!("{}", e))?;

    // Create the functions and data structures related to task management

    // `__set_sleep_flag`, a function that designates that a
    // yielding thread is to be resumed as soon as possible
    let set_sleep_flag = state
        .create_function(set_sleep_flag)
        .map_err(|e| specific_runtime!("{}", e))?;
    globals
        .raw_set("__set_sleep_flag", set_sleep_flag)
        .map_err(|e| specific_runtime!("{}", e))?;

    // `sleep`, a function that yields a thread to be resumed
    // as soon as possible

    let sleep: Function = state
        .load(
            r#"function()
        __set_sleep_flag(true)
        coroutine.yield()
        return
    end"#,
        )
        .eval()
        .map_err(|e| specific_runtime!("{}", e))?;
    globals
        .raw_set("sleep", sleep)
        .map_err(|e| specific_runtime!("{}", e))?;

    // Create the task info table, used to store information about
    // the currently running tasks

    let task_info = state
        .create_table()
        .map_err(|e| specific_runtime!("{}", e))?;
    globals
        .raw_set("__task_info", task_info)
        .map_err(|e| specific_runtime!("{}", e))?;

    // Create the sleep queue, used to store sleeping tasks

    let sleep_queue = state
        .create_table()
        .map_err(|e| specific_runtime!("{}", e))?;
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

    // Set the yield table's __len metamethod to the function
    // that gets the first border
    yield_metatable
        .raw_set("__len", yield_len)
        .map_err(|e| specific_runtime!(e))?;
    yield_table.set_metatable(Some(yield_metatable));
    globals
        .raw_set("__yield_table", yield_table)
        .map_err(|e| specific_runtime!("{}", e))?;

    // Set the exhaustion check as the interrupt handler
    state.set_interrupt(exhaustion_check);
    // Sandbox the state, making the global table and all its subtables readonly
    state
        .sandbox(true)
        .map_err(|e| specific_runtime!("{}", e))?;
    Ok(())
}

/// A CPU limiter for lua states.
fn exhaustion_check() -> LuaResult<VmState> {
    LUA_THREAD_START.with(|start| {
        EXECUTION_LIMIT.with(|limit| {
            if start.borrow().elapsed().as_millis() > *limit.borrow() {
                Err(external!(
                    "execution limit reached - call sleep or coroutine.yield before this point"
                ))
            } else {
                Ok(VmState::Continue)
            }
        })
    })
}

/// Sets the proc path to call when setting
/// a datum's vars using the lua function
/// `datum:set_var`
#[hook("/proc/__lua_set_set_var_wrapper")]
fn set_set_var_wrapper(wrapper: DMValue) {
    wrapper.as_string().and_then(|wrapper_string| {
        SET_VAR_WRAPPER.with(|wrapper| {
            *wrapper.borrow_mut() = Some(wrapper_string);
            Ok(DMValue::null())
        })
    })
}

/// Sets the proc path to call when calling
/// a datum's proc using the lua function
/// `datum:call_proc`
#[hook("/proc/__lua_set_datum_proc_call_wrapper")]
fn set_datum_proc_call_wrapper(wrapper: DMValue) {
    wrapper.as_string().and_then(|wrapper_string| {
        DATUM_CALL_PROC_WRAPPER.with(|wrapper| {
            *wrapper.borrow_mut() = Some(wrapper_string);
            Ok(DMValue::null())
        })
    })
}

/// Sets the proc path to call when calling
/// a global proc using the lua function
/// `dm.global_proc`
#[hook("/proc/__lua_set_global_proc_call_wrapper")]
fn set_global_proc_call_wrapper(wrapper: DMValue) {
    wrapper.as_string().and_then(|wrapper_string| {
        GLOBAL_CALL_PROC_WRAPPER.with(|wrapper| {
            *wrapper.borrow_mut() = Some(wrapper_string);
            Ok(DMValue::null())
        })
    })
}

/// Sets the CPU usage limit, in milliseconds,
/// of each call to lua code
#[hook("/proc/__lua_set_execution_limit")]
fn set_execution_limit(limit: DMValue) {
    limit.as_number().and_then(|limit_num| {
        EXECUTION_LIMIT.with(|execution_limit| {
            *execution_limit.borrow_mut() = (limit_num as u128).to_owned();
            Ok(DMValue::null())
        })
    })
}

/// Creates a new lua state, initializes its variables,
/// and stores it in the `HashMap` of states
#[hook("/proc/__lua_new_state")]
fn new_state() {
    let new_state = Lua::new_with(mlua::StdLib::ALL_SAFE, mlua::LuaOptions::default())
        .map_err(|e| specific_runtime!("{}", e))?;
    static NEXT_STATE_ID: AtomicU32 = AtomicU32::new(1);
    let state_key: String = format!("{}", NEXT_STATE_ID.fetch_add(1, Relaxed));
    apply_state_vars(&new_state, state_key.clone())?;
    STATES.with(|states| {
        states.borrow_mut().insert(state_key.clone(), new_state);
        Ok(DMValue::from_string(state_key).unwrap())
    })
}

/// Looks for the task information corresponding to a lua thread
fn get_task_table_info<'lua, S>(
    lua: &'lua Lua,
    coroutine: Thread<'lua>,
    name: S,
) -> LuaResult<Table<'lua>>
where
    S: Into<String>,
{
    let actual_globals: Table = lua.globals().get("_G")?;
    let task_info_table: Table = actual_globals.raw_get("__task_info")?;
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

/// Handles storing and updating a thread's task info when it yields or finishes
fn handle_coroutine_return<T>(lua: &Lua, coroutine: Thread, name: T) -> LuaResult<i32>
where
    T: Into<String>,
{
    let actual_globals: Table = lua.globals().get("_G")?;
    let task_info_table: Table = actual_globals.raw_get("__task_info")?;

    // Set the task info table to not readonly - we will be modifying it
    task_info_table.set_readonly(false);
    if coroutine.status() == ThreadStatus::Resumable {
        // Check the sleep flag
        let sleep_flag: MluaValue = get_sleep_flag(lua).map_err(|e| specific_external!(e))?;
        if sleep_flag == mlua::Nil {
            // This is a yield -  get the yield table
            let yield_table: Table = actual_globals
                .raw_get("__yield_table")
                .map_err(|e| specific_external!(e))?;

            // Set the yield table to not readonly - we will be modifying it
            yield_table.set_readonly(false);

            // Get the first border - this is where the thread will be put in the yield table
            let first_border = yield_table.len().map_err(|e| specific_external!(e))?;

            // Put the thread in the yield table
            yield_table
                .raw_set(first_border + 1, coroutine.clone())
                .map_err(|e| specific_external!(e))?;

            // Get or create the task info for this thread
            let task_info = get_task_table_info(lua, coroutine, name)?;

            // Set the task info to not readonly - we will be modifying it
            task_info.set_readonly(false);

            // Modify the task info
            task_info.raw_set("status", "yield")?;
            task_info.raw_set("index", first_border + 1)?;

            // Set the task info to readonly - we are done modifying it
            task_info.set_readonly(true);

            // Set the yield table to readonly - we are done modifying it
            yield_table.set_readonly(true);
            return Ok(first_border + 1);
        } else {
            // Clear the sleep flag - We're past the only check for the sleep flag's value
            set_sleep_flag(lua, mlua::Value::Nil)?;

            // This is a sleep - get the sleep queue
            let sleep_queue: Table = actual_globals
                .raw_get("__sleep_queue")
                .map_err(|e| specific_external!(e))?;

            // Set the sleep queue to not readonly - we will be modifying it
            sleep_queue.set_readonly(false);

            // Get the length of the sleep queue - this is the index the thread will be at
            let queue_len = sleep_queue.raw_len();

            // Enqueue the thread into the sleep queue
            sleep_queue
                .raw_set(queue_len + 1, coroutine.clone())
                .map_err(|e| specific_external!(e))?;

            // Get or create the task info for this thread
            let task_info = get_task_table_info(lua, coroutine, name)?;

            // Set the task info to not readonly - we will be modifying it
            task_info.set_readonly(false);

            // Modify the task info
            task_info.raw_set("status", "sleep")?;
            task_info.raw_set("index", queue_len + 1)?;

            // Set the task info to readonly - we are done modifying it
            task_info.set_readonly(true);

            // Set the sleep queue to readonly - we are done modifying it
            sleep_queue.set_readonly(true);
            return Ok(0);
        }
    } else {
        // The task cannot be resumed - remove it from the info table
        task_info_table.raw_remove(coroutine)?;
    }

    // Set the task info table to readonly - we're done modifying it
    task_info_table.set_readonly(true);
    Ok(0)
}

// Loads a snippet of lua source code
#[hook("/proc/__lua_load")]
fn load(state: DMValue, script: DMValue, name: DMValue) {
    let key = state.as_string()?;
    let script_text = script.as_string()?;

    // Use this name for the logging and debugging of this chunk
    let name = name.as_string().unwrap_or_else(|_| String::from("input"));
    STATES.with(|states| {
        let state_map = states.borrow();
        let lua_state = state_map
            .get(&key)
            .ok_or_else(|| specific_runtime!("No lua state at {}", key))?;

        // Load the chunk and set its name
        let mut chunk = lua_state.load(&script_text);
        chunk = err_as_string!(chunk.set_name(&name));

        // Wrap the chunk in a function and create a thread from that function
        let wrapped_function = err_as_string!(chunk.into_function());
        let coroutine = err_as_string!(lua_state.create_thread(wrapped_function));

        // Start the execution timer
        LUA_THREAD_START.with(|start| *start.borrow_mut() = Instant::now());

        // Set `dm.usr` to whatever `usr` currently is in BYOND
        err_as_string!(set_usr(lua_state, usr));

        // Run the thread
        let ret: LuaResult<MultiValue> = coroutine.resume(mlua::Nil);

        // Handle the result
        let status = coroutine.status();
        let yield_index = handle_coroutine_return(lua_state, coroutine, &name)
            .map_err(|e| specific_runtime!(e))?;
        coroutine_result((status, ret, yield_index, name), lua_state)
            .map_err(|e| specific_runtime!(e))
    })
}

#[hook("/proc/__lua_get_globals")]
fn get_globals(state: DMValue) {
    let key = state.as_string()?;
    STATES.with(|states| {
        let state_map = states.borrow();
        let lua_state = state_map
            .get(&key)
            .ok_or_else(|| specific_runtime!("No lua state at {}", key))?;

        // Create the list we will return
        let ret: List = List::new();

        // Get the global table
        let globals: Table = lua_state.globals();

        // Iterate through the global table
        for pair in globals.pairs().filter_map(Result::ok) {
            let (table_key, value): (MluaValue, MluaValue) = pair;

            // Try to convert the table key into a DM value
            let key_value = match LuaModValue::from_lua(table_key.clone(), lua_state) {
                Ok(lua_value) => DMValue::try_from(lua_value).unwrap(),
                Err(_) => {
                    // Cannot directly convert - represent as type name and pointer
                    let typename = table_key.type_name();
                    let key_ref = table_key.to_pointer();
                    DMValue::from_string(format!("{typename}: {key_ref:p}")).unwrap()
                }
            };

            // Try to convert the value to a DM value
            if let Ok(dm_value) = LuaModValue::from_lua(value.clone(), lua_state) {
                let val = DMValue::try_from(dm_value).unwrap();
                ret.set(key_value, val)?;
            } else {
                // Cannot directly convert - represent as type name and pointer
                let typename = value.type_name();
                let value_ref = value.to_pointer();
                let value_string =
                    DMValue::from_string(format!("{typename}: {value_ref:p}")).unwrap();
                ret.set(key_value, value_string)?;
            }
        }
        Ok(DMValue::from(ret))
    })
}

#[hook("/proc/__lua_get_tasks")]
fn get_tasks(state: DMValue) {
    let key = state.as_string()?;
    STATES.with(|states| {
        let state_map = states.borrow();
        let lua_state = state_map
            .get(&key)
            .ok_or_else(|| specific_runtime!("No lua state at {}", key))?;

        // Create the list we will return
        let ret: List = List::new();

        // Get the real, non-proxy global table
        let actual_globals: Table = lua_state
            .globals()
            .get("_G")
            .map_err(|e| specific_runtime!("{}", e))?;

        // Get the task info table
        let task_info: Table = actual_globals
            .raw_get("__task_info")
            .map_err(|e| specific_runtime!("{}", e))?;

        // Iterate through the task info table
        for pair in task_info.pairs().filter_map(Result::ok) {
            let (_, value): (Thread, Table) = pair;

            // Each task info entry is returned as its own assoc list
            let info_list = List::new();

            // Populate the list for the task info entry
            for info_pair in value.pairs() {
                let (info_key, info_value): (DMValue, DMValue) = info_pair.map_or_else(
                    |e| Err(specific_runtime!("{}", e)),
                    |(k, v): (LuaModValue, LuaModValue)| {
                        Ok((DMValue::try_from(k)?, DMValue::try_from(v)?))
                    },
                )?;
                info_list.set(info_key, info_value)?;
            }
            ret.append(info_list);
        }
        Ok(DMValue::from(ret))
    })
}

#[hook("/proc/__lua_call")]
fn call(state: DMValue, function: DMValue, arguments: DMValue) {
    let key = state
        .as_string()
        .map_err(|e| specific_runtime!(e.message))?;

    // Parse the path from the root of the global table to the function being called
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
        let state_map = states.borrow();
        let lua_state = state_map
            .get(&key)
            .ok_or_else(|| specific_runtime!("No lua state at {}", key))?;

        // Validate the arguments to the function
        let arguments_list = arguments.as_list().unwrap_or_else(|_| List::new());
        let func_args: Vec<MluaValue> = (1..=arguments_list.len())
            .map(|i| {
                LuaModValue::try_from(&arguments_list.get(i)?)?
                    .to_lua(lua_state)
                    .map_err(|e| specific_runtime!(e))
            })
            .collect::<Result<Vec<MluaValue>, Runtime>>()?;

        // Produce a human-readable name for the function
        let function_name: String =
            mapped_path
                .clone()
                .iter()
                .try_fold(String::new(), |mut acc, elem| {
                    if !acc.is_empty() {
                        acc += ".";
                    }
                    let value = elem
                        .clone()
                        .to_lua(lua_state)
                        .map_err(|e| specific_runtime!(e))?;
                    let token = String::from_lua(value.clone(), lua_state).unwrap_or_else(|_| {
                        let value_typename = value.type_name();
                        let value_ref = &value;
                        format!("{value_typename}: {value_ref:p}")
                    });
                    acc += token.as_str();
                    Ok(acc)
                })?;

        // Get the global table
        let globals = lua_state.globals();

        // Look for the function at the specified path
        let mut might_be_table_or_func: MluaValue = MluaValue::Table(globals);
        for token in mapped_path {
            if let MluaValue::Table(t) = might_be_table_or_func {
                // `pairs()`, which is used to iterate through the globals list
                // to get its key-value pairs for `get_globals()`, does not
                // invoke metamethods, so we use raw_get for the function search
                might_be_table_or_func = err_as_string!(t.raw_get(token));
            } else {
                break;
            }
        }
        let function = err_as_string!(Function::from_lua(might_be_table_or_func, lua_state));

        // Start the execution timer
        LUA_THREAD_START.with(|start| *start.borrow_mut() = Instant::now());

        // Set `dm.usr` to whatever `usr` currently is in BYOND
        err_as_string!(set_usr(lua_state, usr));

        // Bind the function in a coroutine and run it
        let coroutine = err_as_string!(lua_state.create_thread(function));
        let ret: LuaResult<MultiValue> = coroutine.resume(MultiValue::from_vec(func_args));

        // Handle the result
        let status = coroutine.status();
        let yield_index = err_as_string!(handle_coroutine_return(
            lua_state,
            coroutine,
            &function_name
        ));

        Ok(err_as_string!(coroutine_result(
            (status, ret, yield_index, function_name),
            lua_state
        )))
    })
}

#[hook("/proc/__lua_resume")]
fn resume(state: DMValue, index: DMValue, arguments: DMValue) {
    let key = state
        .as_string()
        .map_err(|e| specific_runtime!(e.message))?;
    STATES.with(|states| {
        let state_map = states.borrow();
        let lua_state = state_map
            .get(&key)
            .ok_or_else(|| specific_runtime!("No lua state at {}", key))?;
        let table_index = index
            .as_number()
            .map_err(|e| specific_runtime!(e.message))? as i32;

        // Validate the arguments
        let arguments_list = arguments.as_list().unwrap_or_else(|_| List::new());
        let resume_args: Vec<MluaValue> = (1..=arguments_list.len())
            .map(|i| {
                LuaModValue::try_from(&arguments_list.get(i)?)?
                    .to_lua(lua_state)
                    .map_err(|e| specific_runtime!(e))
            })
            .collect::<Result<Vec<MluaValue>, Runtime>>()?;

        // Get the task from the yield table
        let actual_globals: Table = err_as_string!(lua_state.globals().get("_G"));
        let yield_table: Table = err_as_string!(actual_globals.raw_get("__yield_table"));
        let coroutine: Thread = err_as_string!(yield_table.raw_get(table_index));

        // Remove the task from the yield table - if the task yields again,
        // it will be put back into the yield table by handle_coroutine_return
        yield_table.set_readonly(false);
        err_as_string!(yield_table.raw_remove(table_index));
        yield_table.set_readonly(true);

        // Get the task's name from the task info table
        let task_info_table: Table = err_as_string!(actual_globals.get("__task_info"));
        let this_tasks_info: Table = err_as_string!(task_info_table.raw_get(coroutine.clone()));
        let function_name = err_as_string!(this_tasks_info.raw_get("name"));

        // Start the execution timer
        LUA_THREAD_START.with(|start| *start.borrow_mut() = Instant::now());

        // Set `dm.usr` to whatever `usr` currently is in BYOND
        err_as_string!(set_usr(lua_state, usr));

        // Run the task
        let ret: LuaResult<MultiValue> = coroutine.resume(MultiValue::from_vec(resume_args));

        // Handle the task's result
        let status = coroutine.status();
        let yield_index = err_as_string!(handle_coroutine_return(
            lua_state,
            coroutine,
            &function_name
        ));

        Ok(err_as_string!(coroutine_result(
            (status, ret, yield_index, function_name),
            lua_state
        )))
    })
}

/// Runs the task at the front of the specified state's sleep queue
#[hook("/proc/__lua_awaken")]
fn awaken(state: DMValue) {
    let key = state
        .as_string()
        .map_err(|e| specific_runtime!(e.message))?;
    STATES.with(|states| {
        let state_map = states.borrow();
        let lua_state = state_map
            .get(&key)
            .ok_or_else(|| specific_runtime!("No lua state at {}", key))?;

        // Get the front task of the sleep queue
        let actual_globals: Table = err_as_string!(lua_state.globals().get("_G"));
        let sleep_queue: Table = err_as_string!(actual_globals.get("__sleep_queue"));
        let coroutine: Thread = err_as_string!(sleep_queue.raw_get(1));

        // Remove the task from the sleep queue
        sleep_queue.set_readonly(false);
        err_as_string!(sleep_queue.raw_remove(1));
        sleep_queue.set_readonly(true);

        // Get the name of the task
        let task_info_table: Table = err_as_string!(actual_globals.get("__task_info"));
        let this_tasks_info: Table = err_as_string!(task_info_table.raw_get(coroutine.clone()));
        let function_name = err_as_string!(this_tasks_info.raw_get("name"));

        // Start the execution timer
        LUA_THREAD_START.with(|start| *start.borrow_mut() = Instant::now());

        // Set `dm.usr` to whatever `usr` currently is in BYOND
        err_as_string!(set_usr(lua_state, usr));

        // Run the task
        let ret: LuaResult<MultiValue> = coroutine.resume(mlua::Value::Nil);

        // Handle the task's result
        let status = coroutine.status();
        let yield_index = err_as_string!(handle_coroutine_return(
            lua_state,
            coroutine,
            &function_name
        ));
        Ok(err_as_string!(coroutine_result(
            (status, ret, yield_index, function_name),
            lua_state
        )))
    })
}

/// Actually produces the return value for hooks that run lua code
fn coroutine_result(
    (status, ret, yield_index, name): (ThreadStatus, LuaResult<MultiValue>, i32, String),
    state: &Lua,
) -> LuaResult<DMValue> {
    let return_list = &List::new();

    // Put the task's name into the return list
    return_list
        .set(
            DMValue::from_string("name").unwrap(),
            DMValue::from_string(name).unwrap(),
        )
        .unwrap();

    // Create a human-readable status string
    let status_string = match status {
        ThreadStatus::Resumable => "yielded",
        ThreadStatus::Unresumable => "finished",
        ThreadStatus::Error => "errored",
    };

    // Put the human-readable status string into the return list
    return_list
        .set(
            DMValue::from_string("status").unwrap(),
            DMValue::from_string(status_string).unwrap(),
        )
        .unwrap();

    // This block handles things that only happen during a yield or sleep
    if status == ThreadStatus::Resumable {
        // Only yields have a non-zero yield index
        if yield_index != 0 {
            return_list
                .set(
                    DMValue::from_string("yield_index").unwrap(),
                    yield_index as f32,
                )
                .unwrap()
        // If the index is zero, this is a sleep - mark it as such and return
        } else {
            return_list
                .set(
                    DMValue::from_string("status").unwrap(),
                    DMValue::from_string("sleeping").unwrap(),
                )
                .unwrap();
            return Ok(DMValue::from(return_list));
        }
    };

    // Parse the result of the task
    let parsed_result = match ret {
        // Pass on the error
        Err(e) => Err(e),
        Ok(return_values) => {
            // Convert the return values to DM values
            let return_value_list = List::new();
            return_values
                .into_iter()
                .try_for_each(|return_value| {
                    let intermediary_value = LuaModValue::from_lua(return_value, state)?;
                    let dm_return_value = DMValue::try_from(intermediary_value)
                        .map_err(|e| specific_external!(e.message))?;
                    return_value_list.append(dm_return_value);
                    Ok(())
                })
                .map(|_| return_value_list)
        }
    };

    // Handle the result of parsing the task result
    match parsed_result {
        Err(e) => {
            // If the status is not an error, this means a return value
            // could not be converted to DM
            if status != ThreadStatus::Error {
                return_list
                    .set(
                        DMValue::from_string("status").unwrap(),
                        DMValue::from_string("bad return").unwrap(),
                    )
                    .unwrap();
            }

            // Regardless, put the error into the param of the return list
            return_list
                .set(
                    DMValue::from_string("param").unwrap(),
                    DMValue::from_string(format!("{e}")).unwrap(),
                )
                .unwrap();
        }
        Ok(result) => {
            return_list
                .set(DMValue::from_string("param").unwrap(), result)
                .unwrap();
        }
    };
    Ok(DMValue::from(return_list))
}

// Kills a task, removing it from the relavent data structures
#[hook("/proc/__lua_kill_task")]
fn kill_task(state: DMValue, task_info: DMValue) {
    let key = state
        .as_string()
        .map_err(|e| specific_runtime!(e.message))?;
    STATES.with(|states| {
        let state_map = states.borrow();
        let lua_state = state_map
            .get(&key)
            .ok_or_else(|| specific_runtime!("No lua state at {}", key))?;
        // Get the task info table
        let actual_globals: Table = lua_state
            .globals()
            .get("_G")
            .map_err(|e| specific_runtime!(e))?;
        let task_info_table: Table = actual_globals
            .raw_get("__task_info")
            .map_err(|e| specific_runtime!(e))?;

        // Convert the info of the task to kill
        // from a DM list to a lua table
        let task_info_list = task_info
            .as_list()
            .map_err(|e| specific_runtime!(e.message))?;
        let task_info_value = lua::tablify_list(task_info_list)?
            .to_lua(lua_state)
            .map_err(|e| specific_runtime!(e))?;
        let target_task_info =
            Table::from_lua(task_info_value, lua_state).map_err(|e| specific_runtime!(e))?;

        // Search the task info table for the info
        // corresponding to the task to kill
        let found_info = task_info_table
            .clone()
            .pairs::<Thread, Table>()
            .collect::<LuaResult<Vec<(Thread, Table)>>>()
            .map_err(|e: mlua::Error| specific_runtime!(e))?
            .into_iter()
            .find(|(_, info)| {
                for info_pair in info.clone().pairs::<MluaValue, MluaValue>().flatten() {
                    let (key, value) = info_pair;
                    if let Ok(info_value) = target_task_info.raw_get::<_, MluaValue>(key) {
                        return info_value == value;
                    }
                }
                false
            });

        match found_info {
            // If we found nothing, just return
            None => Ok(()),

            // If we found a task...
            Some((coroutine, info)) => {
                // Get the index of the task in its relavent table
                let index: i32 = info.raw_get("index").map_err(|e| specific_runtime!(e))?;

                // Check to see if the task is sleeping or yielded
                match info
                    .raw_get::<_, String>("status")
                    .map_err(|e| specific_runtime!(e))?
                    .as_str()
                {
                    // Sleeping task
                    "sleep" => {
                        let sleep_queue: Table = actual_globals
                            .raw_get("__sleep_queue")
                            .map_err(|e| specific_runtime!(e))?;
                        sleep_queue.set_readonly(false);
                        sleep_queue
                            .raw_remove(index)
                            .map_err(|e| specific_runtime!(e))?;
                        sleep_queue.set_readonly(true);
                        Ok(())
                    }

                    // Yielded task
                    "yield" => {
                        let yield_table: Table = actual_globals
                            .raw_get("__yield_table")
                            .map_err(|e| specific_runtime!(e))?;
                        yield_table.set_readonly(false);
                        yield_table
                            .raw_set(index, mlua::Nil)
                            .map_err(|e| specific_runtime!(e))?;
                        yield_table.set_readonly(true);
                        Ok(())
                    }
                    _ => Err(specific_runtime!("invalid task status")),
                }?;

                // Now we can remove the thread itself from the task info table
                task_info_table.set_readonly(false);
                task_info_table
                    .raw_remove(coroutine.clone())
                    .map_err(|e| specific_runtime!(e))?;
                task_info_table.set_readonly(true);
                Ok(())
            }
        }?;

        // Returns null because we have no need for a return value
        Ok(DMValue::null())
    })
}

#[shutdown]
fn shutdown() {
    STATES.with(|states| states.borrow_mut().clear())
}
