# Auxlua

---

## Datums

DM datums are treated as lua userdata, and can be stored in fields. Regular datums are referenced weakly, so if a datum has been deleted, the corresponding userdata will evaluate to `nil` when used in comparisons or functions.

Keep in mind that BYOND can't see that a datum is referenced in a lua field, and will garbage collect it if it is not referenced anywhere in DM.

### datum:get_var(var)

Equivalent to DM's `datum.var`

### datum:set_var(var, value)

Equivalent to DM's `datum.var = value`

### datum:call_proc(procName, ...)

Equivalent to DM's `datum.procName(...)`

---

## Lists

In order to allow lists to be modified in-place across the DM-to-lua language barrier, lists are treated as userdata. Whenever running code that expects a DM value, auxlua will attempt to convert tables into lists.

List references are subject to the same limitations as datum userdata, but you are less likely to encounter these limitations.

### list.len

Equivalent to DM's `list.len`

### list:get(index)

Equivalent to DM's `list[index]`

### list:set(index, value)

Equivalent to DM's `list[index] = value`

### list:add(value)

Equivalent to DM's `list.Add(value)`

### list:to_table()

Converts a DM list into a lua table.

---

## The dm table

The `dm` table consists of the basic hooks into the DM language.

### dm.state_id

The address of the lua state in memory. This is a copy of the internal value used by auxlua to locate the lua state in a global hash map.

### dm.global_proc(proc, ...)

Calls the global proc `/proc/[proc]` with `...` as its arguments.

### dm.world

A reference to DM's `world`, in the form of datum userdata. This reference will never evaluate to `nil`, since `world` always exists.

### dm.global_vars

A reference to DM's `global`, in the form of datum userdata. This reference will never evaluate to `nil`, since `global` always exists.

### dm.usr

A weak reference to DM's `usr`.

---

## Task management

When a main thread calls `sleep()`, it is added to the end of the [`__sleep_queue`](#__sleep_queue) table. Each call to `/proc/__lua_awaken` dequeues and runs the thread at the start of `__sleep_queue`. Under the hood, `sleep` performs the following:

- Sets the global flag [`__sleep_flag`](#__sleep_flag)
- Calls `coroutine.yield()`
- Clears the sleep flag when determining whether the task slept or yielded
- Ignores the return values of `coroutine.yield()` once resumed

When a main thread calls `coroutine.yield()` outside of `sleep()`, it is added to the first free index of the [`__yield_table`](#__yield_table) table. Each call to `/proc/__lua_resume` removes the thread at the specified index of `__yield_table` and resumes it, passing the list `arguments` as the arguments which `coroutine.yield()` will return.

Users of auxlua should provide a system for task management that offers a consistent order of execution for all active tasks within the same state.

---

## Internal globals

Auxlua defines several globals for internal use. These are read-only.

### \_\_sleep_flag

This flag is used to designate that a yielding task should be put in the sleep queue instead of the yield table. Once auxlua determines that a task should sleep, `__sleep_flag` is cleared.

### \_\_set_sleep_flag(value)

A function that sets `__sleep_flag` to `value`. Calling this directly is not recommended, as doing so muddies the distinction between sleeps and yields.

### \_\_sleep_queue

A sequence of threads, each corresponding to a task that has slept. When calling `/proc/__lua_awaken`, auxlua will dequeue the first thread from the sequence and resume it. Threads in this queue can be resumed from lua code, but doing so is heavily advised against.

### \_\_yield_table

A table of threads, each corresponding to a coroutine that has yielded. When calling `/proc/__lua_resume`, auxlua will look for a thread at the index specified in the `index` argument, and resume it with the arguments specified in the `arguments` argument. Threads in this table can be resumed from lua code, but doing so is heavily advised against.

### \_\_task_info

A table of key-value-pairs, where the keys are threads, and the values are tables consisting of the following fields:

- name: A string containing the name of the task
- status: A string, either "sleep" or "yield"
- index: The task's index in `__sleep_queue` or `__yield_table`
  The threads constituting this table's keys can be resumed from lua code, but doing so is heavily advised against.
