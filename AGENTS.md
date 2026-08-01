# Chirp Agent Guide

This file applies to the entire repository. Keep it self-contained: agents should not need to read another repository to learn Chirp's rules.

## Product and Architecture Boundaries

- Chirp is an Emacs UI for X/Twitter. `twitter-cli` owns authentication, network access, API compatibility, and wire-format details; do not add direct X API calls to Chirp.
- `chirp.el` is the public entry point. External users load `(require 'chirp)`.
- `chirp-backend.el` owns `twitter-cli` discovery, process invocation, retries, and JSON envelope handling.
- `chirp-core.el` owns shared state, history, buffer lifecycle, and cross-view navigation.
- `chirp-render.el` renders normalized data. `chirp-media.el` owns cache paths, thumbnail extraction, prefetching, and large-media display.
- View modules orchestrate fetching and rendering; they must not duplicate backend, normalization, or media behavior. `chirp-actions.el` owns compose and write actions, which share one backend request path.

## Change Discipline

- Fix the layer that owns the problem. Name the failing boundary before changing behavior; do not compensate with timing changes, duplicate lookups, or silent fallbacks elsewhere.
- Prefer the smallest coherent implementation. Do not add layers, files, state objects, or compatibility paths for hypothetical future needs.
- Treat helper stacks as debt. Inline trivial one-use wrappers and collapse pass-through ladders; retain a helper only when it owns a complete calculation or workflow and makes its caller materially clearer.
- Aim to keep functions around 30 lines, but do not manufacture tiny wrappers solely to meet a line count. First simplify state, data flow, and control flow.
- Delete unused code and obsolete tests outright. Do not leave deprecated aliases, commented-out paths, or tests that only keep dead helpers alive.
- Refactors must reduce duplication, centralize a real invariant, simplify callers, or improve robustness. Renaming or moving code alone is not enough.
- Read the surrounding implementation, tests, documentation, and integration boundary before changing a user-visible workflow.

## Emacs Lisp Conventions

- The Emacs baseline is 29.1. Verify newer APIs before use and do not raise the baseline without updating package metadata, documentation, and the changelog.
- Every `.el` file uses lexical binding, has the correct package prefix, provides its feature, and ends with the standard footer.
- Public API uses `chirp-`; private implementation uses `chirp--` or the owning module's double-dash prefix. Never call another package's private symbols.
- Use `defvar-local` for per-buffer state, `defcustom` with a precise `:type` and `:group` for user options, and plain `defvar` only for shared process-wide state.
- Loading files must not change the user's active editing behavior. User-facing commands and modes activate behavior explicitly.
- Prefer flat control flow with `if-let*`, `when-let*`, `pcase`, and `pcase-let`. Prefer stock Emacs protocols and primitives over custom frameworks.
- Keep interactive commands thin. Separate data shaping and geometry calculations from process I/O and buffer mutation.
- Public functions, macros, variables, and options require complete docstrings. Document arguments in uppercase and write complete first sentences.
- Require direct runtime dependencies explicitly. Do not rely on transitive loading or use declarations to patch an ownership problem.

## Errors and External Dependencies

- Internal failures must surface. Use `condition-case` or `ignore-errors` only at an explicit boundary around a genuinely recoverable, non-essential operation.
- Use `user-error` for user-caused failures and `error` for broken internal invariants. Error messages state what is wrong.
- Use only public dependency APIs. If a dependency lacks a needed public operation, add or request that API instead of reaching into internals.
- Optional dependencies load at the point of use and must fail clearly when missing or too old; do not silently downgrade behavior.
- Do not require `image-slice` until it is deliberately added to `Package-Requires` from the chosen package source. While Chirp carries the small geometry it needs, keep it confined to the media-grid path. Once the dependency is declared, replace the local duplicate with its public API in the same change.

## Rendering and Media Invariants

- Read-only browsing buffers derive from `special-mode`; compose buffers remain editable. All view state is buffer-local.
- Render from normalized cached data, never by reparsing displayed text. Put durable tweet, author, and media identity in text properties; reserve overlays for ephemeral visuals.
- Timeline, profile, thread, and media views share one browsing buffer and no header line. Keep keys and target resolution consistent across views.
- List views render text first and fill missing avatars or thumbnails asynchronously when possible. Async callbacks must verify that their target buffer is still live and relevant.
- Row-sliced thumbnails use integer pixel geometry, copy image descriptors instead of mutating shared values, cover the prepared source canvas exactly, and join rows with a newline whose `line-height` is `t`.
- Chirp view buffers keep `line-spacing` at zero so adjacent slices remain gapless. Every visible slice and reserved grid cell carries the same media properties as its source item.
- Preserve media-column width on rows below a shorter item so later items do not shift horizontally.
- Any display-geometry change needs a smoke test in a live graphical Chirp buffer; hidden batch buffers cannot validate final font metrics, baselines, wrapping, or seams.

## Tests and Documentation

- Tests must fail when behavior is wrong. Assert public workflows or meaningful invariants, not cosmetic punctuation or private structure without a product contract.
- Completion, hooks, async callbacks, command routing, and other dispatcher bugs need at least one test through the installed or public path.
- Match test weight to the change. Remove duplicate assertions and direct tests of deleted helpers.
- User-visible behavior, defaults, keys, and configuration update `README.md` in the same change. Release-relevant features and fixes also update the Unreleased section of `CHANGELOG.md`.
- Keep each semantic Markdown paragraph or bullet on one source line unless code or a table requires otherwise.

## Pre-Commit Gates

- Read every changed line and run `git diff --check`. Search for dead symbols, accidental private API use, and generated `.elc`, backup, or lock files.
- New or changed definitions must add no `checkdoc` warnings. Do not expand unrelated pre-existing warning cleanup into a focused change.
- Byte-compilation must finish with zero warnings. Remove generated `.elc` files after the check.
- Run the complete ERT suite, not a single test file:

```bash
emacs -Q -batch --eval '(setq load-prefer-newer t)' -L . -L lisp -l ert \
  -l test/chirp-actions-test.el -l test/chirp-backend-test.el \
  -l test/chirp-media-test.el -l test/chirp-notifications-test.el \
  -l test/chirp-profile-test.el -l test/chirp-render-test.el \
  -l test/chirp-thread-test.el -l test/chirp-timeline-test.el \
  --eval '(ert-run-tests-batch-and-exit)'

emacs -Q -batch --eval '(setq load-prefer-newer t)' -L . -L lisp \
  -f batch-byte-compile chirp.el lisp/*.el test/*.el
```

- Before publishing as an ELPA/MELPA package, run `checkdoc` and `package-lint` across every distributable `.el` file with the main package file configured; release with zero warnings.
