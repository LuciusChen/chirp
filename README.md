# chirp.el

`chirp.el` is a small Emacs browser for X/Twitter. It uses [`twitter-cli`](https://github.com/public-clis/twitter-cli) for authentication and data fetching, then renders each view in its own `special-mode` buffer.

## Scope

This first cut is intentionally narrow:

- Home timeline
- Following timeline
- Bookmarks
- Liked tweets for the current account
- Lists
- Search
- Tweet thread/detail
- Profile header with a direct posts stream
- Profile follower/following lists
- Basic write actions via a `transient` menu: post, reply, quote, retweet,
  like, bookmark, follow, and unfollow

It does not implement DMs, notifications, or paid features.

## Requirements

- GNU Emacs 29.1 or newer
- `twitter-cli` installed and available as `twitter` or `twitter-cli`

If you want the `twitter-cli` fork/version currently used by Chirp before
upstream merges land, install the `stable` branch from this fork:

```bash
uv tool install --force "git+https://github.com/LuciusChen/twitter-cli.git@stable"
```

If you keep a local checkout and want Chirp to follow that checkout directly:

```bash
cd ~/repos/twitter-cli
git switch stable
uv tool install --force -e .
```

The editable install is convenient for local development, but remember that the
active CLI will follow whatever branch that checkout is currently on.

Chirp auto-detects either executable name by default. If your binary lives
elsewhere, customize:

```elisp
(setq chirp-cli-command "/path/to/twitter")
```

If Emacs does not inherit your shell `PATH`, Chirp also checks a few common
user bin directories such as `~/.local/bin`. You can extend that list with:

```elisp
(setq chirp-cli-search-paths
      '("~/.local/bin" "~/bin" "/some/other/bin"))
```

## Load

```elisp
(add-to-list 'load-path "~/chirp")
(require 'chirp)
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
- `RET`: open the current tweet or profile, or open large media when point is on a thumbnail
- In profile summaries, `RET` on `Followers` or `Following` opens that user list
- In profile buffers, `RET` on the subview strip switches between available profile timelines
- `m`: open the first media item for the current tweet
- `D`: download the current media, or choose one media item from the current tweet; photos try the original-resolution URL and videos use the highest-quality variant
- `A`: open the author profile
- `x`: open the actions menu for timeline switching, your own profile, bookmarks, liked tweets, lists, post/reply/quote, follow/unfollow, and tweet actions
- `o`: open the current item in a browser
- `q`: close the current Chirp buffer

Inside the compose buffer:

- `C-c C-a`: attach an image file (up to 4)
- `C-c C-v`: paste one image from the clipboard
- `C-c C-d`: remove an attached image
- `C-c C-c`: close the draft immediately and send it in the background
- `C-c C-k`: cancel the draft

Tweet metrics also reflect the current local state: liked tweets show `Liked`,
bookmarked tweets show `Saved`, and retweeted tweets show `RTed`.
Clipboard image paste uses `wl-paste` on Wayland and `pngpaste` on macOS when available.

## Media

- Images render as small thumbnails in timeline, thread, and profile post lists.
- Thumbnails are rendered without inserted gaps, so point can move between adjacent images directly.
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
- Timeline "load more" uses `twitter-cli feed --cursor` and appends older posts without re-fetching the already loaded prefix.
- This repository was bootstrapped without `twitter-cli` installed locally, so the code is byte-compile checked but not end-to-end runtime tested yet.
