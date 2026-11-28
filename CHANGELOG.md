# Changelog

This file records user-visible Chirp changes from 2026-08-01 onward. Earlier
history remains available in Git.

## Unreleased

- Chirp now connects to X directly. Open `M-x chirp` in a graphical Emacs with Xwidgets, sign in to X, and Chirp uses that session. The `twitter-cli` dependency has been removed.

### Breaking Changes

- Rename the home command from `chirp-home` to `chirp`. Update any key bindings that use the old name.

### Added

- Browse dated account activity with `M-x chirp-notifications` (with semantic labels by event type) and load older events with `N`. Use `/` to search with minibuffer history and press `TAB` in search buffers to toggle `Top` and `Latest`. Clicking text places point, while handles (including `retweeted by` and `replying to`), hashtags, links, and media open their targets.
- Bind direct single-key shortcuts (`l`, `R`, `B`, `c`, `r`, `Q`, `T`, `y`, `i`) in Chirp browsing views, apply optimistic state updates with rollback on failure for like/repost/bookmark toggles, and subtly highlight the active entry header line (`chirp-highlight-current-entry`).
- Show compact local timestamps, article headings and links, visible and filtered conversation counts, a visible margin for quoted posts, and tweet separators only between independent posts rather than inside adjacent reply chains.
- Show the weighted length, reply context and attachment previews while composing.
- Load additional pages of bookmarks and replies in thread views with `N` or by moving past the last entry.
- Inline tweet translation from the actions menu with `T`.
- Native desktop notifications for account activity.
- Complete user handles while composing posts. Suggestions arrive asynchronously through the active completion frontend.
- Gapless row-sliced image and video/GIF cover thumbnails.
- Configurable case-insensitive filtering of reply content and author nicknames/handles, with conservative Chinese-first literal and all-fragment defaults collected from real public reply spam.
- Highlighted context labels for related tweets and reply-target handles in thread views.
- Inline `Show more` expansion for article previews while `RET` elsewhere on a tweet continues to open its detail thread.
- Persistent user spam phrases and keywords in a plain-text rule file, with `S`/`C-u S` capture from reply content or author names, one shared match scope, and a structured upstream submission form.
- Mouse-1 controls on visible tweet reply, repost/retweet, like, and bookmark metrics, routed through the existing tweet action commands.

### Fixed

- Restore timeline avatars for authors whose name and profile image are supplied separately by X.
- Open a fresh timeline after WebKit sign-in and recover a timeline read interrupted by the final redirect.
- Preserve full NoteTweet text, focus tweets in deep threads, related-post labels, and follow relationships from X’s native responses.
- Continue searches, likes, lists, and follower lists beyond page one. Skip duplicate-only feed pages and preserve the active window while appending older entries.
- Open the list picker only while its original window and buffer are selected and the minibuffer is free. Reopening a failed home timeline retries the request.
- Keep a new sign-in active when an old request reports expired credentials. The background X session no longer prompts when quitting Emacs.
- Keep usable results when X returns partial data and time out stalled requests.
- Open playable video variants, validate image size before reading, and use the target file size for symlinked attachments.
- Keep the rest of an oversized timeline page locally so loading older posts still works when X reorders the feed. Refreshing Home or Following preserves the continuation for the posts already on screen.
- Append new entries in Home, Following and Bookmarks without redrawing earlier pages or repeating media prefetch for those entries.
- Keep the compose draft and its attachments after a failed send. Use `C-c C-o` to check your posts before retrying.
- Preserve the current view on a failed refresh.
- Tweet and media permalinks no longer appear as trailing links, while genuine external links highlight on hover.
- When a post, reply, or quote fails, report whether X rejected it or the outcome is unknown. Chirp never retries publishing automatically.
- Likes views now show every returned tweet with its like state active.
- Reply filtering now recognizes collected affiliate, dating, and drug-spam nickname templates, including the shared `返佣` marker and the combined `FoxLink` + `银狐` signature.
