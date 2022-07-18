# Changelog

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
