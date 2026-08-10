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
- Use lowercase hyphenated Lisp names. Single-word predicates end in `p`; multi-word predicates end in `-p`. Prefix intentionally unused lexical variables and arguments with `_`.
- Prefer face names without a redundant `-face` suffix for new APIs, but preserve Chirp's established public face naming instead of renaming customization symbols only for style.
- Let Emacs indentation be authoritative. Use spaces rather than hard tabs, keep trailing parentheses together, separate unrelated top-level forms with a blank line, and keep lines near 80 characters when that improves readability without awkwardly splitting clear strings, URLs, or forms.
- Use `when` for one positive branch, `unless` for one negated branch, `not` for boolean negation, and `null` specifically for the empty list. Do not add a redundant `progn`. Prefer chained comparisons and `1+`/`1-` idioms.
- Use `defvar-local` for per-buffer state, `defcustom` with a precise `:type` and `:group` for user options, and plain `defvar` only for shared process-wide state.
- Loading files must not change the user's active editing behavior. User-facing commands and modes activate behavior explicitly.
- Prefer flat control flow with `if-let*`, `when-let*`, `pcase`, and `pcase-let`. Prefer stock Emacs protocols and primitives over custom frameworks.
- Prefer `cl-loop` over `dolist` plus manual accumulation for non-trivial iteration. Do not use `mapcar` when its result is discarded; use `dolist` for multi-form side effects and `seq-do` for applying one existing function.
- Prefer idiomatic primitives over reconstructed equivalents, such as `vconcat` for vectors and direct predicate results instead of `(not (null ...))`.
- Prefer `let*`, `pcase-let`, alists/plists, small helpers, or table-driven mappings for short-lived context. Avoid more than three or four positional parameters; use `cl-defun` keyword arguments or one documented plist/alist when order becomes hard to remember. Reserve structs or object layers for stable state crossing module or lifecycle boundaries.
- Use `#'function-name` for executable function values. Use lambdas only for genuinely local behavior; hooks, keymaps, advice, customizable callbacks, and other long-lived registrations normally use named functions. Do not wrap a function in a lambda that only forwards the same arguments.
- Do not write a macro when a function suffices. Keep macros as thin syntax layers over testable functions, design the call form first, use backquote/unquote, prevent capture with generated symbols, and never evaluate arguments more often than the contract permits. Every macro declares an Edebug `debug` specification and, when appropriate, an `indent` declaration.
- Keep interactive commands thin. Separate data shaping and geometry calculations from process I/O and buffer mutation.
- Public functions, macros, variables, and options require complete docstrings. Document arguments in uppercase and write complete first sentences.
- Require direct runtime dependencies explicitly. Do not rely on transitive loading or use declarations to patch an ownership problem.

## Errors and External Dependencies

- Internal failures must surface. Use `condition-case` or `ignore-errors` only at an explicit boundary around a genuinely recoverable, non-essential operation.
- Use `user-error` for user-caused failures and `error` for broken internal invariants. Error messages state what is wrong.
- Do not wrap standard errors without adding behavior whose semantics are named in the wrapper's docstring.
- Use only public dependency APIs. If a dependency lacks a needed public operation, add or request that API instead of reaching into internals.
- Load libraries with `require`, not `load` or `load-library`. Require `cl-lib` explicitly when using its functions, and do not put runtime-needed dependencies behind `eval-when-compile`.
- Add `declare-function` or `defvar` only at an intentionally lazy or cyclic compile-time boundary. Do not repeat contracts supplied by a mandatory `require`, and do not use declarations to patch an ownership problem.
- Optional dependencies load at the point of use and must fail clearly when missing or too old; do not silently downgrade behavior.
- Compatibility shims stay under Chirp's private prefix; prefer a prefixed `defalias` when the upstream function exists. Avoid `with-eval-after-load` except for an optional integration registered at a clear package boundary.
- Do not require `image-slice` until it is deliberately added to `Package-Requires` from the chosen package source. While Chirp carries the small geometry it needs, keep it confined to the media-grid path. Once the dependency is declared, replace the local duplicate with its public API in the same change.

## Modes, Autoloads, and Completion

- Read-only UI buffers derive from `special-mode`; editing buffers derive from the appropriate editing parent. Major modes keep their state buffer-local and register buffer-local hooks with LOCAL=`t` in the mode body.
- Add `;;;###autoload` to user-facing `M-x` commands and minor modes, not to private helpers, variables, maps, hooks, or internal modes. Internal modes, maps, and hooks use the owning subsystem's double-dash prefix.
- Use `completing-read` for interactive selection unless a specific product requirement calls for a custom reader.
- Completion-at-point functions stay close to the Emacs protocol: compute bounds and candidates directly, return the standard completion list, and avoid a separate context model unless real call paths share it. CAPFs return quickly, use `:exclusive 'no` when composing, avoid blocking or re-entrant synchronous work, and are added buffer-locally with LOCAL=`t`.

## Rendering and Media Invariants

- Read-only browsing buffers derive from `special-mode`; compose buffers remain editable. All view state is buffer-local.
- Render from normalized cached data, never by reparsing displayed text. Put durable tweet, author, and media identity in text properties; reserve overlays for ephemeral visuals.
- Timeline, profile, thread, and media views use dedicated Chirp buffers, creating fresh buffers by default and reusing them only for explicit in-place transitions. Keep shared browsing conventions such as no header line, consistent keys, and target resolution across views.
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
- Documentation claims that state commands, capabilities, compatibility, metrics, benchmarks, or trust signals need evidence from code, manifests, tests, workflows, or release records; label claims that remain unverified.
- Keep documentation-only work documentation-only: do not change product code, configuration, CI, or dependencies merely to make a documentation claim true unless explicitly requested.
- Substantial `README.md` changes open with what the project is, who it serves, the problem it solves, and the reader's next action, and keep installation and Quick Start easy to find. Lead with user outcomes; avoid vague promotional claims, feature piles, and unnecessary badges or calls to action.
- Use a `Breaking Changes` section only for real installation, API, or configuration breaks; omit empty sections instead of writing `None`.
- Keep each semantic Markdown paragraph or bullet on one source line unless code or a table requires otherwise.

## Pre-Commit Gates

- Read every changed line and run `git diff --check`. Search for dead symbols, accidental private API use, and generated `.elc`, backup, or lock files.
- Byte-compile every distributable `.el` file with zero warnings. Remove generated `.elc` files after the check.
- Run `checkdoc` with zero warnings across all distributable `.el` files, not only the main entry file. Public definitions have complete docstrings whose first line is a complete sentence ending in a period; document arguments in uppercase and in use order without visually aligning continuation lines inside help text.
- Run `package-lint` with zero warnings across all distributable `.el` files with `package-lint-main-file` configured to `chirp.el`; do not duplicate package metadata in implementation files.
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

- Keep package metadata only in `chirp.el`: `Author`, `URL`, `Version`, and complete direct `Package-Requires` minimum versions including Emacs. Split implementation files retain formal license metadata. Main and implementation headers use concise package descriptions, and every file ends with its matching standard footer.
