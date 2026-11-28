# Changelog

This file records user-visible Chirp changes from 2026-08-01 onward. Earlier history remains available in Git.

## Unreleased

- Connect to X directly without `twitter-cli`, using a single native `url.el` transport, pure-Elisp `X-Client-Transaction-Id` signing for GraphQL writes, `chirp-x-query-id-overrides` for hot-fixing rotated GraphQL IDs, zero-dependency local browser cookie import with running/default browser prioritization, expiration filtering, multi-profile failover, and clipboard `Copy as cURL` fallback (Firefox, Zen, LibreWolf, Chrome, Dia, Arc, Brave, Edge, Chromium, Vivaldi), and unified authentication across GUI, terminal, and SSH Emacs (`M-x chirp-login`, `M-x chirp-logout`, `~/.emacs.d/chirp/auth.json`, `auth-source`, or environment variables), adapted from [`emacs-apps/chirp`](https://github.com/emacs-apps/chirp).

### Breaking Changes

- Rename `chirp-home` to `chirp`. Update any key bindings that call the old name.

### Added

- Browse account activity with `M-x chirp-notifications` (`i`), with distinct faces per event type, timestamps, and `N` pagination, plus optional desktop notifications (`chirp-notifications-mode`).
- Search X directly with `/` (with minibuffer history) and press `TAB` in search buffers to toggle between `Top` and `Latest`.
- Direct single-key bindings (`l`, `R`, `B`, `c`, `r`, `Q`, `T`, `y`, `i`) in Chirp browsing views, immediate local updates with rollback on failure for like/repost/bookmark toggles, and active-entry header highlighting (`chirp-highlight-current-entry`).
- Copy opened photos to the system clipboard with `c` or `w` in media views (AppKit on macOS, `wl-copy` on Wayland, `xclip` on X11).
- Mouse-1 click support on tweet metrics (reply, repost, like, quote, bookmark), handles (including `retweeted by` and `replying to`), hashtags, quote headings, links, media, and profile tabs, while clicking ordinary post text places point.
- Compact local timestamps, article headings and links, inline `Show more` article expansion, visible and filtered reply counts in threads, and a visual margin for quoted posts.
- Weighted character count, header-line shortcut hints, shaded reply/quote context cards, on-demand attachment previews, discard confirmation on non-empty drafts, native macOS AppKit clipboard paste, and asynchronous `@handle` completion while composing.
- Inline tweet translation (`T` or `x T`).
- Pagination with `N` (or moving past the last entry with `n`) across timelines, bookmarks, searches, lists, profiles, threads, notifications, and follower lists.
- Gapless row-sliced image and video/GIF cover thumbnails.
- Upgraded spam and mute filtering (`S` / `C-u S` / `~/.emacs.d/chirp/spam-rules.txt`) with OP/self/following exemptions, author bio and quoted-tweet matching, zero-width and fullwidth anti-evasion normalization, `@handle`, `host:domain`, `A && B`, and `/regexp/` rule syntax, clickable expansion of hidden thread replies with rule labels, and personal mute rule application across Home, Following, Search, and List views.

### Fixed

- Sign GraphQL write mutations (including posts, replies, and quotes) with pure-Elisp `X-Client-Transaction-Id` headers, browser `User-Agent` / `X-Client-UUID` headers, and canonical quote `attachment_url` paths to prevent X error 226 rejections, and unify authentication across GUI, terminal, and SSH Emacs by removing the separate WebKit session stack.
- Show the quota and retry headers X supplies with posting restrictions and rate-limit errors.
- Update the X operation used to publish standard posts, replies, and quotes to match the current web client.
- Keep pagination on the selected search ordering while switching between Top and Latest.
- Keep pending likes, reposts, and bookmarks consistent across open views. Restore their displayed state together if X rejects the action.
- Preserve reply links across page boundaries and use the explicit parent post ID when available.
- Copy photos with the clipboard tool for the active Linux display session and release entry highlights on redraw.
- Omit tweet separators inside adjacent reply chains (including sibling replies) so dividers appear only between independent posts.
- Render multi-media thumbnails at a uniform row height with rounded borders, horizontal gaps, and a single-row `+N` overflow badge when more items exist than fit in the window (press `RET` and `n`/`p` to browse all items).
- Restore timeline avatars for authors whose name and profile image are supplied separately by X.
- Open a fresh timeline buffer when signing in with a different X account.
- Preserve full NoteTweet text, focus tweets in deep threads, related-post labels, and follow relationships from X's native responses.
- Continue searches, likes, lists, and follower lists past page one; skip duplicate-only feed pages and keep the selected window when appending older entries.
- Open the `M-x chirp-list` picker only while its requesting window and buffer remain active and the minibuffer is free.
- Reopen a failed home timeline by retrying the request instead of reusing the broken buffer, and preserve the current view when a refresh fails.
- Keep usable partial data from X responses and time out stalled requests.
- Open playable video variants, validate image size before reading, and check target file size for symlinked attachments.
- Retain unshown entries from oversized timeline pages locally so pagination and Home/Following refreshes keep working when X reorders the feed.
- Append new entries in Home, Following, and Bookmarks without redrawing earlier pages or repeating media prefetch.
- Retain the compose draft and attachments after a failed send, report whether X rejected a write or the outcome is unknown, and never auto-retry publishing.
- Hide tweet and media permalinks from trailing link lists while keeping genuine external links highlighted on hover.
- Mark every tweet returned by the Likes view as liked.
- Expand default reply spam rules to catch additional affiliate, dating, and drug-spam nickname patterns.
