# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/).

## Unreleased
### Added
- `reaper-start-timer-and-quit-window`
- Autoload `reaper`
- Sort projects/tasks in last used order when using ivy

### Changed
- Starting new timer now defaults to last used project and task
- RET now closes the window after starting timer
- SPC start tracking on entry at point

### Fixed
- Editing an entry fetches project/tasks first, if not already
  fetched
- Compatibility with `display-line-numbers-mode`
- Use a `kill-buffer-hook` to cancel timer
- Make point stick around when deleting entries

## 1.0.0 - 2019-08-28
### Added
- Initial implementation
