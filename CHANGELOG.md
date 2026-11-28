# Changelog

This file records user-visible Chirp changes from 2026-08-01 onward. Earlier history remains available in Git.

## Unreleased

- Connect to X directly without `twitter-cli`, with the native X web API transport and adapters adapted from [`emacs-apps/chirp`](https://github.com/emacs-apps/chirp). Open `M-x chirp` in a graphical Emacs with Xwidgets, sign in on the X page, and Chirp reuses that session.

### Breaking Changes

- Rename `chirp-home` to `chirp`. Update any key bindings that call the old name.

### Added

- Browse account activity with `M-x chirp-notifications` (`i`), with distinct faces per event type, timestamps, and `N` pagination, plus optional desktop notifications (`chirp-notifications-mode`).
- Search X directly with `/` (with minibuffer history) and press `TAB` in search buffers to toggle between `Top` and `Latest`.
- Direct single-key bindings (`l`, `R`, `B`, `c`, `r`, `Q`, `T`, `y`, `i`) in Chirp browsing views, immediate local updates with rollback on failure for like/repost/bookmark toggles, and active-entry header highlighting (`chirp-highlight-current-entry`).
- Copy opened photos to the system clipboard with `c` or `w` in media views (AppKit on macOS, `wl-copy` on Wayland, `xclip` on X11).
- Mouse-1 click support on tweet metrics (reply, repost, like, quote, bookmark), handles (including `retweeted by` and `replying to`), hashtags, quote headings, links, media, and profile tabs, while clicking ordinary post text places point.
- Compact local timestamps, article headings and links, inline `Show more` article expansion, visible and filtered reply counts in threads, and a visual margin for quoted posts.
- Weighted character count, reply/quote preview, attachment thumbnails, and asynchronous `@handle` completion while composing.
- Inline tweet translation (`T` or `x T`).
- Pagination with `N` (or moving past the last entry with `n`) across timelines, bookmarks, searches, lists, profiles, threads, notifications, and follower lists.
- Gapless row-sliced image and video/GIF cover thumbnails.
- Case-insensitive reply spam filtering with persistent local rules (`S` / `C-u S`), a shared match scope across text, links, names, and handles, and an issue form for proposing built-in rules.

### Fixed

- Continue a rejected quote from its original post on X while keeping the local draft and attachments.
- Keep pending WebKit callbacks alive across garbage collection on macOS, including when opening a profile from a tweet.
- Restore an existing WebKit X session quietly in the background after restarting Emacs, showing the sign-in page only when authentication is needed or when `M-x chirp-login` is run explicitly.
- Keep pagination on the selected search ordering while switching between Top and Latest.
- Keep pending likes, reposts, and bookmarks consistent across open views. Restore their displayed state together if X rejects the action.
- Preserve reply links across page boundaries and use the explicit parent post ID when available.
- Copy photos with the clipboard tool for the active Linux display session and release entry highlights on redraw.

- Omit tweet separators inside adjacent reply chains (including sibling replies) so dividers appear only between independent posts.
- Wrap multi-photo thumbnail grids into separate rows within the window width instead of letting `visual-line-mode` wrap individual slices.
- Restore timeline avatars for authors whose name and profile image are supplied separately by X.
- Open a fresh timeline after WebKit sign-in and recover reads interrupted by the final redirect.
- Preserve full NoteTweet text, focus tweets in deep threads, related-post labels, and follow relationships from X's native responses.
- Continue searches, likes, lists, and follower lists past page one; skip duplicate-only feed pages and keep the selected window when appending older entries.
- Open the `M-x chirp-list` picker only while its requesting window and buffer remain active and the minibuffer is free.
- Reopen a failed home timeline by retrying the request instead of reusing the broken buffer, and preserve the current view when a refresh fails.
- Keep a new sign-in active when an older request fails with expired credentials, and stop the background WebKit session from prompting on Emacs exit.
- Keep usable partial data from X responses and time out stalled requests.
- Open playable video variants, validate image size before reading, and check target file size for symlinked attachments.
- Retain unshown entries from oversized timeline pages locally so pagination and Home/Following refreshes keep working when X reorders the feed.
- Append new entries in Home, Following, and Bookmarks without redrawing earlier pages or repeating media prefetch.
- Retain the compose draft and attachments after a failed send, report whether X rejected a write or the outcome is unknown, and never auto-retry publishing.
- Hide tweet and media permalinks from trailing link lists while keeping genuine external links highlighted on hover.
- Mark every tweet returned by the Likes view as liked.
- Expand default reply spam rules to catch additional affiliate, dating, and drug-spam nickname patterns.
