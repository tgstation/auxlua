# Changelog

## [1.4.2]

### Added

- Auxlua comes bundled with [auxcov](https://www.github.com/willox/auxtools/tree/master/auxcov), a library for DM code coverage.

### Fixes

- Bumps the version dependency of auxtools, adding support for up to 515.1606 on all platforms.

### Changes

- Auxlua is now built with LTO (link time optimization)

## [1.4.1]

### Fixes

- Bumps the version dependency of auxtools, adding support for 515.1602 on Windows.

## [1.4.0]

### Added

- Adds the `over_exec_usage` function, which returns `true` if the current thread has been running longer than the specified fraction of the execution limit (defaults to 0.95 if not specified).

### Changes

- Refactored the execution limit to allow for setting it to any positive finite number.

## [1.3.2]

### Fixed

- Actually fixes linux function signature scanning for BYOND 514.1588.

## [1.3.1]

### Fixed

- Updated auxtools with a patch to fix linux function signature scanning for BYOND 514.1588.

## [1.3.0]

### Added

- Adds the `loadstring` function. It compiles a string into lua code - if the code compiles, `loadstring` will return a function that runs that code; otherwise, it returns `nil` and the error that prevented the code from compiling.

## [1.2.1]

### Fixed

- Running an empty string (zero bytes long, not just all whitespace) no longer crashes Dream Daemon.

## [1.2.0]

### Changed

- Citing compatibility concerns, the `__index` and `__newindex` metamethods for datums and lists have been moved behind a `vars` field for datums and an `entries` field for lists.

## [1.1.1]

### Fixed

- Fixed getting datum-tied lists not working. (I can't believe I overlooked this)

## [1.1.0]

### Added

- Adds support for directly accessing and modifying the following types of lists:

  - args
  - vis_contents
  - vis_locs
  - world.contents
  - image overlays
  - image underlays

- Adds an `is_null` method for all DM userdata types, which returns the truthiness of the DM value passed into it.

- Datum vars and list keys can now be get and set using the `__index` and `__newindex` metamethods. `datum:get_var`, `datum:set_var`, `list:get`, and `list:set` are kept as legacy functions, but the existing methods for datums and lists will shadow datum vars and assoc list keys with the same name. You are still able to get and set otherwise shadowed variables using the mentions mentioned prior.
- The length of DM lists can now be obtained using the `__len` metamethod. `list.len` is kept as a legacy field, but will shadow the `len` key of assoc lists that have one.

### Fixed

- Converting a recursive table to/from lua no longer causes a stack overflow.

### Changed

- You can no longer directly pass the following types of lists to lua functions from DM, as they are attached to datums, and as such, are not guaranteed to remain valid for the entire time they are referenced in lua (this restriction does not apply to `global.vars`, `world.vars`, or `world.contents`, as they will always be valid):

  - vars
  - overlays
  - underlays
  - vis_contents
  - vis_locs
  - contents

- The sleep queue, yield table, and task info table are now registry values, as there is no need for users to access them from lua code. If you need the first empty index in the yield table, you can access it with the global field `__next_yield_index`.
- `dm.usr` and `dm.state_id` are now registry values, but are still accessible through the `dm` table using its `__index` metamethod.
- `sleep` is now a native function.
- Internally, DM userdata is now cached in a manner that results in userdata instances corresponding to the same underlying DM value being strictly equal. This is important because table indexing only tests by strict equality. As a result of this change, you can now properly index tables with datums and list references.

### Removed

- Removed the `__set_sleep_flag()` function, as it was only intended for use in `sleep`, which now no longer has any need for it.

## [1.0.0]

### Added

- Added `of_type` method to DM lists, which filters by typepath. For instance, `mob_list:of_type("/mob/living/carbon/human")`.
- Added an `__iter` metamethod to DM lists, meaning you no longer need to call `to_table()` when iterating.

## [0.2.1]

### Changed

- Error logging for auxlua's internal mechanisms should be much more verbose.
- Instead of runtiming when failing to set `dm.usr`, auxlua will first attempt to print the error using the `print` function. Failing that, it will call `world.Error` with the error message. This should now make it possible to unshadow `_G` if it was previously shadowed.

## [0.2.0]

### Fixed

- Fixes a crash that occured when calling `print` when there is more than 1 lua state.

## [0.1.5]

### Changed

- On Windows, the dll file is no longer pinned to the process that loads it - it will be unloaded upon calling `auxtools_full_shutdown` and subsequently closing the world that initialized it.

### Added

- In the event of a panic, said panic will be output to a log file. If a DM global var of the name `log_directory` is specified at init-time, the panic log will be located at `[log_directory]/auxtools_panic.log`. Otherwise, it will be located at `auxtools_panic_[UNIX_TIMESTAMP].log`.

## [0.1.4]

### Fixed

- Fixed a crash when trying to call a proc on `dm.global_vars`.
- Fixed a crash when trying to add a var to a `vars`-type list.
- Fixed directly setting a value in a `vars`-type list being usable to bypass the set-var wrapper.

## [0.1.3]

### Added

- Implements the `__tostring` metamethod for DM types.
- Adds the hook `__lua_set_print_wrapper`, allowing you to set a DM proc as a wrapper for the `print` function.

## [0.1.2]

### Changed

- Instead of failing to convert, Lua values unrepresentable in DM will be converted to strings in a manner consistent with Lua's `print` function.

### Fixed

- Fixed a crash caused by inappropriate code for calling the datum var setting wrapper.
- In the event that a lua value somehow fails to convert to a DM value when read by `__lua_get_globals`, the pointer will actually refer to the value's location in memory rather than the address of the Rust wrapper.

## [0.1.1]

### Changed

- The CPU limit exhaustion error now correctly specifies `sleep` instead of `dm.sleep`

### Fixed

- Fixed all created states having the same internal ID, leading there to be only one state in practice.
- Fixed a bug causing states to reach the execution limit before even having a chance to run their code.

## [0.1.0]

- Initial Release
