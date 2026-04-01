# chirp Development Guide

This file captures the parts of `clutch`'s development rules that fit `chirp`.
Keep it practical. If a rule does not help this codebase, do not cargo-cult it.

## Core Principles

- Keep Chirp small. Do not add abstraction layers, files, or indirection for hypothetical future needs.
- Prefer one consistent workflow over overlapping commands. Timeline switching, compose flows, and tweet actions should converge on one clear path each.
- Diagnose the failing layer before changing behavior. Fix backend invocation, state normalization, rendering, or media loading in the layer that owns it.
- Delete dead code instead of leaving compatibility shims or commented-out paths.

## Architecture

- `chirp.el` is the public entry point. External users should load `(require 'chirp)`.
- `chirp-backend.el` owns `twitter-cli` discovery, process invocation, retries, and JSON envelope handling.
- `chirp-core.el` owns shared state, history, buffer lifecycle, and cross-view navigation helpers.
- `chirp-render.el` renders normalized data into buffers. Data-bearing annotations belong in text properties, not overlays.
- `chirp-media.el` owns cache paths, thumbnail extraction, async prefetch, and large-media display behavior.
- `chirp-timeline.el`, `chirp-thread.el`, and `chirp-profile.el` orchestrate view-specific fetching and rendering. They should not duplicate backend or media logic.
- `chirp-actions.el` owns transient actions and compose workflows. Write actions should share one backend request path.

## Emacs Lisp Conventions

- Emacs baseline is 29.1. Do not silently raise it.
- Read-only browsing buffers derive from `special-mode`. Editable compose buffers derive from an editable parent mode.
- Use `defvar-local` for buffer state, `defcustom` for user options, and plain `defvar` only for shared process-wide state.
- Keep interactive commands thin. Separate pure data shaping from buffer mutation.
- Prefer flat control flow with `if-let*`, `when-let*`, `pcase`, and `pcase-let`.
- Surface errors at the user boundary. Do not swallow internal failures with broad fallback returns.
- Public functions, variables, and user options need real docstrings. Byte-compilation must stay warning-free.

## UX Invariants

- Browsing stays in one shared Chirp buffer that is renamed per view.
- Timeline, profile, thread, and media views do not use a header line.
- Key behavior should stay consistent across timeline, thread, profile, and media views.
- List views should render text first and fill avatars/thumbnails asynchronously when possible.
- User-visible keybinding or workflow changes must update `README.md` in the same change.

## Repository Hygiene

- Do not commit `.elc` files, backup files, or editor lock files.
- Before committing, read the full diff.
- Before committing, run:

```bash
emacs -Q -batch --eval '(setq load-prefer-newer t)' -L . -L lisp -l chirp.el
emacs -Q -batch --eval '(setq load-prefer-newer t)' -L . -L lisp -f batch-byte-compile chirp.el lisp/*.el test/*.el
emacs -Q -batch -L . -L lisp -l ert -l test/chirp-actions-test.el \
  --eval '(ert-run-tests-batch-and-exit)'
```
