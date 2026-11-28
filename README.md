# chirp.el

`chirp.el` is an Emacs client for X/Twitter. It fetches data directly from X and renders timelines, threads, profiles, notifications, and media in dedicated `special-mode` and `image-mode` buffers, with editable compose buffers for posting.

## Quick Start

Chirp requires GNU Emacs 29.1 or newer with built-in libxml HTML support (and built-in SQLite for automatic browser cookie import). Authentication works identically in graphical Emacs, terminal Emacs (`emacs -nw`), and remote SSH sessions.

Run `M-x chirp` (or `M-x chirp-login`): Chirp automatically imports your existing `x.com` session cookies from a local browser — checking currently running browsers and your system default browser first, filtering expired cookies, and failing over across browser profiles if a stored session is stale (Firefox, Zen, LibreWolf, Dia, Arc, Chrome, Brave, Edge, Chromium, or Vivaldi — using only Emacs's built-in SQLite plus standard OS crypto tools, with no external CLI or third-party dependencies), verifies them against X, and saves them to `~/.emacs.d/chirp/auth.json` (`0600` permissions; customizable via `chirp-x-auth-file`). On headless machines, Windows, Safari, or when no local browser session is available, `M-x chirp-login` accepts `Copy as cURL` or `Cookie` headers directly from the clipboard or masked manual entry. Use `C-u M-x chirp-login` to pick a specific browser profile, clipboard import, or manual paste, set `chirp-x-cookie-browser` to pin your default browser, set `chirp-x-query-id-overrides` if X rotates a GraphQL query ID, and run `M-x chirp-logout` to remove the saved file.

You can also provide credentials through environment variables (`TWITTER_AUTH_TOKEN` and `TWITTER_CT0`) or `auth-source` (`host x.com`, users `auth_token` and `ct0`). Lookup order is environment variables, then `chirp-x-auth-file`, then `auth-source`.

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
M-x chirp-logout
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
- `y`: copy the tweet URL.
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
- `C-c C-v`: paste an image from the clipboard (uses `pngpaste` or built-in macOS AppKit on macOS, `wl-paste` on Wayland, or `xclip` on X11).
- `C-c C-d`: remove an attached image.
- `M-TAB`: complete `@handle` mentions via your active completion frontend (with built-in completion, press `M-TAB` again if the first lookup is still loading).
- `C-c C-c`: send the draft in the background and return to browsing; if sending fails, the draft and attachments are restored (`C-c C-o` opens your profile to check before retrying).
- `C-c C-k`: cancel the draft (asks for confirmation when you have entered text or attached images).

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

## Spam & Mute Filtering

Chirp filters spam replies in thread views and applies your personal mute rules across Home, Following, Search, List, and Thread views. Built-in spam defaults apply only to thread replies and automatically exempt the focus tweet, the thread author's own replies, `Related tweet` entries, your own posts, and accounts you follow. Matching inspects text, author bio, display name, handle, expanded links, and quoted tweets after stripping zero-width characters and normalizing whitespace and fullwidth ASCII.

When replies are hidden in a thread, press `RET` or click the `── Conversation · N shown · M hidden (RET to show) ──` heading to toggle showing them with their matched rule labels.

Press `S` on a post to save a rule to `~/.emacs.d/chirp/spam-rules.txt` (one rule per line; `#` comments and blank lines ignored) and refresh immediately. Use `C-u S` to prefill `@handle` for the current author, or run `M-x chirp-thread-edit-spam-rules` to edit the file directly. Supported rule forms:

- `phrase`: case-insensitive literal substring
- `@handle`: exact author handle mute
- `host:t.me`: link hostname filter (matches exact host and subdomains)
- `phrase1 && phrase2`: combined rule requiring every fragment
- `/regexp/`: regular expression match

You can change the file location via `chirp-thread-spam-rules-file`.

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

The native X web API backend (`lisp/chirp-x.el`, the `X-Client-Transaction-Id` protocol in `lisp/chirp-x-transaction.el`, and the X GraphQL/REST adapters in `lisp/chirp-backend.el`) was adapted and ported from the [`emacs-apps/chirp`](https://github.com/emacs-apps/chirp) fork. Many thanks to its maintainers for building the native backend work that made dropping `twitter-cli` possible.

## License

Chirp is available under the [MIT License](LICENSE).
