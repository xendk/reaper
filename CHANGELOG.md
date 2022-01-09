# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/).

## Unreleased
### Added
- `e d` to edit entry description
- `e t` to edit entry task
- `e p` to edit entry project
- Simple calculations in time entry
- `reaper-get-running-timer-note` for getting note of running timer.
- `!` to clear project/task cache

### Changed
- `reaper-edit-entry` moved from `e` to `e e`

## 1.2.1 - 2020-11-14
### Fixed
- Fix `reaper-goto-date`

## 1.2.0 - 2020-11-14
### Added
- Display total daily time
- Add `f`/`b` to go back/forwards a day

## 1.1.0 - 2019-09-21
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
