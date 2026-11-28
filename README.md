# chirp.el

`chirp.el` is an Emacs client for X/Twitter. It fetches data directly from X and renders timelines, threads, profiles, notifications, and media in dedicated `special-mode` and `image-mode` buffers, with editable compose buffers for posting.

## Quick Start

Chirp requires GNU Emacs 29.1 or newer. In a graphical Emacs built with Xwidgets, open any Chirp view and sign in on the X page; once sign-in finishes, Chirp opens the view you asked for and keeps the WebKit session in a background buffer. Closing that session buffer asks for confirmation, and `M-x chirp-login` signs in again.

In a terminal-only Emacs, set `TWITTER_AUTH_TOKEN` and `TWITTER_CT0` in the environment, or store `auth_token` and `ct0` entries for `x.com` in `auth-source`.

### Load Chirp

```elisp
(add-to-list 'load-path "~/chirp")
(require 'chirp)
```

Run `M-x chirp` to open the For You timeline.

## Entry Points

```elisp
M-x chirp
M-x chirp-login
M-x chirp-notifications
M-x chirp-following
M-x chirp-bookmarks
M-x chirp-likes
M-x chirp-me
M-x chirp-list      ;; choose from your lists in the minibuffer
M-x chirp-search
M-x chirp-thread
M-x chirp-profile
M-x chirp-profile-followers
M-x chirp-profile-following-users
```

If you switch away while `M-x chirp-list` is loading, run it again to pick from the cached lists.

## Keys

- `g`: refresh the current view (on Home and Following, newer posts are merged at the top without clearing the buffer).
- `TAB`: switch between Home and Following, toggle between `Top` and `Latest` in search buffers, or cycle profile tabs (`Posts`, `Replies`, `Highlights`, `Media`, `Likes`).
- `n` / `p`: move to the next or previous entry; at the end of a paginated view, `n` loads the next page.
- `N`: load older entries in timelines, searches, lists, profiles, threads, notifications, and follower lists (also retries a failed page load).
- `q`: close the current window; Home and Following buffers stay alive so you can switch back to them later.
- `RET`: open the item at point (a tweet's thread, a profile, a large media view, a link, or inline `Show more` expansion for article previews).
- `/`: search X (with minibuffer history; press `TAB` in search results to toggle `Top` and `Latest`).
- `l` / `R` / `B`: toggle like, repost, or bookmark on the tweet at point; open views preview the new count and revert together if the request fails.
- `c` / `r` / `Q`: compose a new post, reply to the tweet at point, or quote it.
- `T`: translate the tweet at point and display the translation underneath.
- `y`: copy the tweet URL as a `fixupx.com` link.
- `i`: open the account activity view.
- `m`: open the first media item on the current tweet.
- `D`: download the media at point, or pick one item from the current tweet (originals for photos, highest-quality variant for videos).
- `A`: open the author's profile.
- `S`: add a spam filter phrase from the active region or current tweet text (`C-u S` starts from the author's display name).
- `x`: open the transient actions menu.
- `o`: open the current item in a browser.
- Mouse: clicking post text (including quoted text) places point; clicking handles, hashtags, quote headings, links, thumbnails, profile tabs, or tweet action metrics activates that target.

Inside the compose buffer:

- `C-c C-a`: attach an image file (up to 4).
- `C-c C-v`: paste an image from the clipboard (uses `pngpaste` on macOS and `wl-paste` on Wayland when available).
- `C-c C-d`: remove an attached image.
- `M-TAB`: complete `@handle` mentions via your active completion frontend (with built-in completion, press `M-TAB` again if the first lookup is still loading).
- `C-c C-c`: send the draft in the background and return to browsing; if sending fails, the draft and attachments are restored (`C-c C-o` opens your profile to check before retrying).
- `C-c C-k`: cancel the draft.

Drafts longer than 280 weighted characters are sent as long-form posts automatically.

## Notifications

Run `M-x chirp-notifications` (or press `i`) to browse account activity. Use `n` and `p` to move between events, `RET` anywhere on a linked event to open its post, and `N` to load older activity.

To enable periodic desktop notifications:

```elisp
(chirp-notifications-mode 1)
```

The first check records a baseline without notifying on old items; subsequent checks run every five minutes by default (freedesktop notifications over D-Bus on Linux, AppleScript on macOS).

```elisp
(setq chirp-notifications-interval 300
      chirp-notifications-max-results 20)
```

## Translation

Tweet translation (`T` or `x T`) defaults to Chinese. Change the target language with:

```elisp
(setq chirp-translation-language "zh")
```

## Thread Reply Filtering

Chirp filters common spam replies in thread views using a case-insensitive list of literal phrases matched against reply text, expanded links, author names, and `@handles`. The focus tweet and X's `Related tweet` entries are never filtered. Nested lists match only when every fragment appears.

Press `S` on a reply to save a literal phrase to `~/.emacs.d/chirp/spam-rules.txt` (one entry per line, `#` comments and blank lines ignored) and refresh the view immediately. Use `C-u S` to prefill the author's display name, or run `M-x chirp-thread-edit-spam-rules` to edit the file directly. You can change the file location via `chirp-thread-spam-rules-file`.

```elisp
(setq chirp-thread-spam-keywords nil) ; Disable filtering.

;; Or customize rules in Elisp (nested lists require all fragments to match):
(setq chirp-thread-spam-keywords
      '("联系我领取"
        ("体制内幼师" "sao的很")
        "check my bio asappp"
        "t.me/"))
```

Refresh an open thread with `g` after changing `chirp-thread-spam-keywords` or editing the rules file. To contribute patterns to the built-in list in [`lisp/chirp-spam-rules.el`](lisp/chirp-spam-rules.el), use the [spam rule submission form](https://github.com/LuciusChen/chirp/issues/new?template=spam-rule.yml).

## Appearance

Independent posts in list views are separated by a divider line (adjacent reply chains stay grouped without a divider between them). To customize or disable the separator:

```elisp
(setq chirp-tweet-separator "- - - - - - - - - - - -"
      chirp-tweet-separator-indent 6)
```

Set `chirp-tweet-separator` to `nil` or `""` to turn separators off. To disable highlighting on the active entry's header line:

```elisp
(setq chirp-highlight-current-entry nil)
```

Avatars and media thumbnails can be toggled independently:

```elisp
(setq chirp-show-avatars nil
      chirp-show-tweet-media nil)
```

When thumbnails are hidden, Chirp shows compact text placeholders (including alt text when available) so media keys still work.

## Media

- Tweet permalinks and raw image/video URLs are hidden from the link list; external links stay visible and highlight on hover.
- Photo and video/GIF thumbnails are sliced by text row so point moves smoothly row by row, and multi-photo grids wrap cleanly within the window width.
- List views display cached avatars and thumbnails immediately and prefetch missing ones in the background (video/GIF covers use upstream previews or `ffmpeg` when available).
- Press `RET` on a thumbnail to open a photo in `image-mode`, or play a video through `mpv` (falling back to the browser when `mpv` is unavailable).
- Inside the media viewer, press `n` / `p` to switch items, `c` or `w` to copy the current photo to the clipboard, `D` to download, `o` to open in a browser, and `q` to close.

Common media and cache options:

```elisp
(setq chirp-video-player-window-size '(1280 . 720)
      chirp-media-download-directory "~/Downloads/chirp/"
      chirp-backend-read-cache-ttl 15)
```

Point `chirp-video-player-command` at another player or set it to `nil` to always open videos in a browser. Set `chirp-media-render-from-cache-only` to `nil` to fetch thumbnails synchronously, or `chirp-media-prefetch-images` to `nil` to disable background prefetching.

## Notes

- X can change its web API without notice. Reading bookmarks works, while creating a bookmark currently returns HTTP 404.

## Acknowledgments

The native X web API backend (`lisp/chirp-x.el` and the X GraphQL/REST adapters in `lisp/chirp-backend.el`) was adapted and ported from the [`emacs-apps/chirp`](https://github.com/emacs-apps/chirp) fork. Many thanks to its maintainers for building the native backend work that made dropping `twitter-cli` possible.

## License

Chirp is available under the [MIT License](LICENSE).
