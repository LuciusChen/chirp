# Changelog

This file records user-visible Chirp changes from 2026-08-01 onward. Earlier
history remains available in Git.

## Unreleased

- Move X access into Emacs. Configure the X session in auth-source or the environment.

### Added

- Load additional pages of bookmarks and replies in thread views with `N` or by moving past the last entry.
- Inline tweet translation from the actions menu with `T`.
- Native desktop notifications for account activity.
- User-handle completion while composing posts.
- Gapless row-sliced image and video/GIF cover thumbnails.
- Configurable case-insensitive filtering of reply content and author nicknames/handles, with conservative Chinese-first literal and all-fragment defaults collected from real public reply spam.
- Highlighted context labels for related tweets and reply-target handles in thread views.
- Inline `Show more` expansion for article previews while `RET` elsewhere on a tweet continues to open its detail thread.
- Persistent user spam phrases and keywords in a plain-text rule file, with `S`/`C-u S` capture from reply content or author names, one shared match scope, and a structured upstream submission form.
- Mouse-1 controls on visible tweet reply, repost/retweet, like, and bookmark metrics, routed through the existing tweet action commands.

### Fixed

- Append new entries in Home, Following and Bookmarks without redrawing earlier pages or repeating media prefetch for those entries.

- Keep the compose draft and its attachments after a failed send. Use `C-c C-o` to check your posts before retrying.
- Fetch mention suggestions asynchronously and preserve the current view on a failed refresh.

- Mention completion no longer moves point back to the `@` character.
- Tweet and media permalinks no longer appear as trailing links, while genuine external links highlight on hover.
- Publishing failures no longer trigger automatic post, reply, or quote retries; structured non-retryable errors such as X code `344` now reach the user directly.
- Likes views now show every returned tweet with its like state active.
- Reply filtering now recognizes collected affiliate, dating, and drug-spam nickname templates, including the shared `返佣` marker and the combined `FoxLink` + `银狐` signature.
