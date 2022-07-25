# Changelog

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
