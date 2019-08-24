# Reaper

A simple Harvest client.

## Motivation

While there is already an [Emacs Harvest
client](https://github.com/kostajh/harvest.el) it might not be to
everybodys liking:

* It primary uses [Hydra](https://github.com/abo-abo/hydra) and
  [Ivy](https://github.com/abo-abo/swiper) for its user interface.
* It fetches data from Harvest on each interaction.
* It uses the Harvest V1 API, which is deprecated and unsupported.

Reaper on the other hand:

* Uses a tabulated-list-mode buffer and completing-read (which Ivy can
  still override).
* Only fetch time entries when starting up, when you ask it to and
  when changing something. Yet it still displays the proper time for a
  running timer.
* Uses the Harvest V2 API.

## Installation

Either using package.el or:

``` emacs-lisp
(use-package reaper
  :ensure t ; If using package.el, alternatively :straight t.
  :bind ("C-c h" . reaper))
```

## Setup

Create a personal access token at https://id.getharvest.com/developers
and customize `reaper-api-key` to the given value. On the same page
you should be able to see the account id you'll need for
`reaper-account-id`.

## Usage

Use `M-x reaper` to open the buffer (or whatever key sequence you've bound it to). 

Bindings in the buffer:
* `n/p` - next/previous entry.
* `q` - bury buffer. It can be recalled with the `reaper` command.
* `g` - refresh the list.
* `d` - change date. Supply a date in Y-M-D or Y.M.D format. Year
  or year and month may be left out.
* `RET` - restart the timer at point.
* `c` - start a new timer.
* `s` - stop running timer.
* `k/DEL` - delete the entry at point.
* `e` - edit project/task/note of entry at point.
* `t` - edit time for entry at point (even works for running timers).
* `Q` - to kill reaper buffer. Stops timers and clears all local data.

# What's in the name

Harvest was obviously taken. Reaper alludes to Grim Reaper which I
sorta have the same kind of relationship with. You know he's there,
you know he's unavoidable, but that doesn't mean you have to like him.
