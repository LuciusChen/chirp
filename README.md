# chirp.el

`chirp.el` is an Emacs browser for X/Twitter for people who want to read, search, inspect, and act on posts without leaving Emacs. Browsing views use dedicated Emacs buffers; read-only text views use Chirp's `special-mode` UI, image views use `image-mode`, and compose buffers remain editable. Chirp delegates authentication and data fetching to [`twitter-cli`](https://github.com/public-clis/twitter-cli).

## What you can do

Use Chirp to follow timelines, search and inspect posts, move from a post to its thread, author, profile lists, or media, and perform common post, reply, quote, like, bookmark, and follow actions without leaving Emacs. Optional desktop notifications surface account activity, and translation is available on demand.

Chirp is a client around `twitter-cli`, not a replacement for it: authentication, X API compatibility, and network behavior belong to the CLI. Direct messages are not implemented, and publishing outcomes and account limits remain dependent on the external CLI and X.

## Quick Start

Chirp requires GNU Emacs 29.1 or newer and `twitter-cli` installed and available as `twitter` or `twitter-cli`.

### Install `twitter-cli`

If you want the `twitter-cli` fork/version currently used by Chirp before upstream merges land, install the `stable` branch from this fork:

```bash
uv tool install --force "git+https://github.com/LuciusChen/twitter-cli.git@stable"
```

If you keep a local checkout and want Chirp to follow that checkout directly:

```bash
cd ~/repos/twitter-cli
git switch stable
uv tool install --force -e .
```

The editable install is convenient for local development, but remember that the active CLI will follow whatever branch that checkout is currently on.

Chirp auto-detects either executable name by default. If your binary lives elsewhere, customize:

```elisp
(setq chirp-cli-command "/path/to/twitter")
```

If Emacs does not inherit your shell `PATH`, Chirp also checks a few common user bin directories such as `~/.local/bin`. You can extend that list with:

```elisp
(setq chirp-cli-search-paths
      '("~/.local/bin" "~/bin" "/some/other/bin"))
```

### Load Chirp

```elisp
(add-to-list 'load-path "~/chirp")
(require 'chirp)
```

Run `M-x chirp-home` to open the For You timeline in a dedicated buffer.

## Notifications

To receive account activity notifications, enable the global mode:

```elisp
(chirp-notifications-mode 1)
```

The first check establishes a baseline without showing old activity. Later
checks run every five minutes by default and notify only unseen activity. Linux
uses freedesktop notifications over D-Bus; macOS uses AppleScript. Titles and
bodies have Emacs text properties removed before reaching either backend.

To change the interval or the size of each recent-activity check:

```elisp
(setq chirp-notifications-interval 300
      chirp-notifications-max-results 20)
```

Tweet translation defaults to Chinese. Customize the target language with:

```elisp
(setq chirp-translation-language "zh")
```

## Entry Points

```elisp
M-x chirp-home
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

## Keys

- `g`: refresh; on Home and Following, Chirp keeps the current timeline visible and merges newer posts at the top
- `TAB`: switch between Home and Following on those timelines; in profile buffers, cycle `Posts`, `Replies`, `Highlights`, `Media`, and `Likes` when available
- `n` / `p`: next or previous entry; on Home and Following, `n` on the last entry loads more older posts
- `N`: load more older posts on Home and Following
- `q`: close the current Chirp window; on For You and Following, keep the timeline buffer alive so you can switch back later
- When Home or Following has no more older posts, Chirp says so instead of leaving the last loading message in place
- `RET`: expand an article when point is on `Show more`; otherwise open the current tweet or profile, or open large media when point is on a thumbnail
- In profile summaries, `RET` on `Followers` or `Following` opens that user list
- In profile buffers, `RET` on the subview strip switches between available profile timelines
- `m`: open the first media item for the current tweet
- `D`: download the current media, or choose one media item from the current tweet; photos try the original-resolution URL and videos use the highest-quality variant
- `A`: open the author profile
- `S`: add a persistent spam phrase or keyword using the active region or current tweet text; use `C-u S` to start with the author's display name
- `x`: open the actions menu for timeline switching, your own profile, bookmarks, liked tweets, lists, post/reply/quote, follow/unfollow, translation, and tweet actions
- `x T`: translate the tweet at point and show the result below its original text
- `o`: open the current item in a browser

Inside the compose buffer:

- `C-c C-a`: attach an image file (up to 4)
- `C-c C-v`: paste one image from the clipboard
- `C-c C-d`: remove an attached image
- `M-TAB`: complete a user handle after typing an `@` prefix (also available
  to completion-at-point frontends such as Corfu)
- `C-c C-c`: close the draft immediately and send it in the background
- `C-c C-k`: cancel the draft

With the `twitter-cli` `stable` branch above, Premium accounts can send drafts
over the standard 280 weighted-character limit. Chirp passes the complete text
through unchanged, and `twitter-cli` automatically selects its long-form
posting operation for posts, replies, and quotes.

Tweet metrics also reflect the current local state: liked tweets show `Liked`,
bookmarked tweets show `Saved`, and retweeted tweets show `RTed`.
Every tweet returned by the Likes view is shown with its like state active, even when twitter-cli omits the per-item `favorited` field.
Clipboard image paste uses `wl-paste` on Wayland and `pngpaste` on macOS when available.

## Thread Reply Filtering

Chirp hides likely spam replies using a conservative default list collected from repeated public reply spam, prioritizing Chinese templates before English ones. Matching configured literal phrases against reply text, expanded links, the author's display name, and the author's `@handle` ignores case and never filters the thread's focus tweet. A nested list requires every fragment to occur, which catches split templates without filtering on either broad fragment alone; set the option to nil to disable filtering.

Press `S` on a reply to edit and save a literal phrase, or select the useful portion first so it becomes the initial input. Use `C-u S` to start from the author's display name. Chirp stores accepted entries under `user-emacs-directory` (`~/.emacs.d/chirp/spam-rules.txt` in the usual setup), one UTF-8 literal per line, de-duplicates them without regard to case, and immediately refreshes the current view. Blank lines and lines beginning with `#` are ignored. Nicknames, handles, reply text, and expanded links all use this same combined rule set.

Run `M-x chirp-thread-edit-spam-rules` to edit the file directly, or customize `chirp-thread-spam-rules-file` to keep it in another location such as a dotfiles repository. After manual edits, refresh an open thread with `g`. Complex rules requiring every fragment to occur remain available through `chirp-thread-spam-keywords` and the source-controlled defaults.

To propose local rules for everyone, open the [spam rule submission form](https://github.com/LuciusChen/chirp/issues/new?template=spam-rule.yml), paste one or more entries from the local file, and include public examples or other evidence. The same submitted rule covers reply text and author identity, so it does not need separate nickname and content variants. Built-in additions are reviewed for false positives; maintainers can keep a specific literal rule or turn broad fragments into an all-fragment rule in [`lisp/chirp-spam-rules.el`](lisp/chirp-spam-rules.el).

```elisp
(setq chirp-thread-spam-keywords nil) ; Disable filtering entirely.

;; Or use Elisp for grouped or fully customized in-memory rules.
(setq chirp-thread-spam-keywords
      '("联系我领取"
        ("体制内幼师" "sao的很")
        "check my bio asappp"
        "t.me/"))
```

Refresh an open thread after changing the option.

X-provided related-tweet modules remain visible with a highlighted `Related tweet` context label and are not treated as replies by the keyword spam filter. Reply-target `@username` handles are highlighted separately from their muted `replying to` context, while standard quote tweets continue to render as nested `Quoted …` blocks.

## Appearance

Tweet lists use a lightweight separator between posts by default. To customize it:

```elisp
(setq chirp-tweet-separator "- - - - - - - - - - - -")
(setq chirp-tweet-separator-indent 6)
```

Set it to `nil` or an empty string to disable tweet separators.

Avatars and tweet media thumbnails can be hidden independently:

```elisp
(setq chirp-show-avatars nil)
(setq chirp-show-tweet-media nil)
```

When tweet media thumbnails are hidden, Chirp keeps compact text media entries
so media commands still work, and shows alt text when twitter-cli provides it.

## Media

- Chirp hides tweet permalinks and image/video resource links; genuine external links remain visible and highlight on hover.
- Images render as small thumbnails in timeline, thread, and profile post lists.
- Images and video/GIF cover thumbnails are split into gapless text-row slices, so point can move through a tall cover one row at a time. Multiple media items remain aligned in the same thumbnail grid.
- Timeline, thread, and profile views now render cached avatars/thumbnails first; missing media are prefetched in the background so text appears faster.
- Video and animated GIF thumbnails are filled in asynchronously when Chirp can use an upstream preview image or extract one with `ffmpeg`.
- Press `RET` on a thumbnail to open the photo in a new Chirp media buffer when image display is available.
- In image and fallback media views, `q` closes the current media buffer and `D` downloads the current media item.
- Videos currently open externally through `mpv` when available, or the browser otherwise.

If you want a larger or fixed mpv window, customize:

```elisp
(setq chirp-video-player-window-size '(1280 . 720))
```

If you do not use `mpv`, either point `chirp-video-player-command` at another
player executable, or set it to `nil` to always open video URLs in the browser.

Downloaded media default to `~/Downloads/`.  To change that:

```elisp
(setq chirp-media-download-directory "~/Downloads/chirp/")
```

If you prefer the old blocking behavior, customize:

```elisp
(setq chirp-media-render-from-cache-only nil)
```

To disable background image prefetch, customize:

```elisp
(setq chirp-media-prefetch-images nil)
```

To trade freshness for faster repeated opens of the same thread/profile/article,
customize the short in-memory backend cache:

```elisp
(setq chirp-backend-read-cache-ttl 15)
```

## Notes

- The package expects `twitter-cli --json` to return the documented envelope from `SCHEMA.md`.
- The parser is deliberately defensive because upstream X payloads can drift.
- Chirp retries explicit network and server failures only for safely repeatable requests; post, reply, and quote commands are never retried automatically because a lost response can leave the publishing outcome unknown.
- X application error `344` is a posting-window limit rather than a transport failure; with the current `twitter-cli` stable branch, Chirp reports it without retrying so the account can wait for the window to reset.
- Timeline "load more" uses `twitter-cli feed --cursor` and appends older posts without re-fetching the already loaded prefix.
- Automated tests exercise the `twitter-cli` JSON and process boundary without sending live X writes; account limits and network behavior remain external to the test suite.

## License

Chirp is available under the [MIT License](LICENSE).
