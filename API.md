# Auxlua

---

## Datums

DM datums are treated as lua userdata, and can be stored in fields. Due to fundamental limitations in lua, userdata is inherently truthy. Since datum userdata can correspond to a deleted datum, which would evaluate to `null` in DM, the function [`datum:is_null()`](#datumisnull) is provided to offer a truthiness test consistent with DM.

Keep in mind that BYOND can't see that a datum is referenced in a lua field, and will garbage collect it if it is not referenced anywhere in DM.

### datum:get_var(var)

Equivalent to DM's `datum.var`

### datum:set_var(var, value)

Equivalent to DM's `datum.var = value`

### datum:call_proc(procName, ...)

Equivalent to DM's `datum.procName(...)`

### datum:is_null()

This function is used to evaluate the truthiness of a DM var. The lua statement `if datum:is_null() then` is equivalent to the DM statement `if(!datum)`.

### datum.vars

Returns a userdatum that allows you to access and modifiy the vars of a DM datum by index. `datum.vars.foo` is equivalent to `datum:get_var("foo")`, while `datum.vars.foo = bar` is equivalent to `datum:set_var("foo", bar)`

---

## Lists

In order to allow lists to be modified in-place across the DM-to-lua language barrier, lists are treated as userdata. Whenever running code that expects a DM value, auxlua will attempt to convert tables into lists.

List references are subject to the same limitations as datum userdata, but you are less likely to encounter these limitations for regular lists.

Some lists (`vars`, `contents`, `overlays`, `underlays`, `vis_contents`, and `vis_locs`) are inherently attached to datums, and as such, their corresponding userdata contains a weak reference to the containing datum. Use [`list:is_null`](#listisnull) to validate these types of lists.

### list.len

Equivalent to DM's `list.len`

### list:get(index)

Equivalent to DM's `list[index]`

### list:set(index, value)

Equivalent to DM's `list[index] = value`

### list:add(value)

Equivalent to DM's `list.Add(value)`

### list:remove(value)

Equivalent to DM's `list.Remove(value)`

### list:to_table()

Converts a DM list into a lua table.

### list:of_type(type_path)

Will extract only values of type `type_path`.

### list:is_null()

A similar truthiness test to [`datum:is_null()`](#datumisnull). This function only has the possibility of returning `false` for lists that are inherently attached to a datum (`vars`, `contents`, `overlays`, `underlays`, `vis_contents`, and `vis_locs`).

### list.entries

Returns a userdatum that allows you to access and modifiy the entries of the list by index. `list.entries.foo` is equivalent to `list:get("foo")`, while `list.entries.foo = bar` is equivalent to `list:set("foo", bar)`

---

## The dm table

The `dm` table consists of the basic hooks into the DM language.

### dm.state_id

The address of the lua state in memory. This is a copy of the internal value used by auxlua to locate the lua state in a global hash map. `state_id` is a registry value that is indirectly obtained using the `dm` table's `__index` metamethod.

### dm.global_proc(proc, ...)

Calls the global proc `/proc/[proc]` with `...` as its arguments.

### dm.world

A reference to DM's `world`, in the form of datum userdata. This reference will never evaluate to `nil`, since `world` always exists.

### dm.global_vars

A reference to DM's `global`, in the form of datum userdata. This reference will never evaluate to `nil`, since `global` always exists.

### dm.usr

A weak reference to DM's `usr`. This is a registry value that is indirectly obtained using the `dm` table's `__index` metamethod.

---

## Execution Limit

In order to prevent freezing Dream Daemon with infinite loops, auxlua enforces an execution limit, defaulting to 100ms. When a single lua state has been executing for longer than this limit, it will eventually stop and produce an error.

To avoid exceeding the execution limit, call `sleep()` or `coroutine.yield()` before the execution limit is reached.

### over_exec_usage(fraction)

This function returns whether the current run of the Lua VM has executed for longer than the specified fraction of the execution limit. You can use this function to branch to a call to `sleep()` or `coroutine.yield()` to maximize the amount of work done in a single run of the Lua VM. If nil, `fraction` will default to 0.95, otherwise, it will be clamped to the range \[0, 1\].

### /proc/\_\_lua_set_execution_limit(time)

This DM hook adjusts the execution limit. `time` is a positive number of milliseconds.

---

## Task management

When a main thread calls `sleep()`, it is added to the end of the [`sleep_queue`](#sleep_queue) table. Each call to `/proc/__lua_awaken` dequeues and runs the thread at the start of `sleep_queue`. Under the hood, `sleep` performs the following:

- Sets the [`sleep_flag`](#__sleep_flag)
- Calls `coroutine.yield()`
- Clears the sleep flag when determining whether the task slept or yielded
- Ignores the return values of `coroutine.yield()` once resumed

When a main thread calls `coroutine.yield()` outside of `sleep()`, it is added to the first free index of the [`yield_table`](#__yield_table) table. Each call to `/proc/__lua_resume` removes the thread at the specified index of `yield_table` and resumes it, passing the list `arguments` as the arguments which `coroutine.yield()` will return.

Users of auxlua should provide a system for task management that offers a consistent order of execution for all active tasks within the same state.

---

## Internal globals

Auxlua defines several registry values for each state. Note that there is no way to access registry values from lua code.

### sleep_flag

This flag is used to designate that a yielding task should be put in the sleep queue instead of the yield table. Once auxlua determines that a task should sleep, `sleep_flag` is cleared.

### sleep_queue

A sequence of threads, each corresponding to a task that has slept. When calling `/proc/__lua_awaken`, auxlua will dequeue the first thread from the sequence and resume it.

### yield_table

A table of threads, each corresponding to a coroutine that has yielded. When calling `/proc/__lua_resume`, auxlua will look for a thread at the index specified in the `index` argument, and resume it with the arguments specified in the `arguments` argument.

### task_info

A table of key-value-pairs, where the keys are threads, and the values are tables consisting of the following fields:

- name: A string containing the name of the task
- status: A string, either "sleep" or "yield"
- index: The task's index in `sleep_queue` or `yield_table`
