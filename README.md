# chirp.el

`chirp.el` is a small Emacs browser for X/Twitter. It uses [`twitter-cli`](https://github.com/public-clis/twitter-cli) for authentication and data fetching, then renders the results in a single shared `special-mode` buffer that is renamed to match the current view.

## Scope

This first cut is intentionally narrow:

- Home timeline
- Following timeline
- Bookmarks
- Search
- Tweet thread/detail
- Profile with recent posts
- Basic write actions via a `transient` menu: post, reply, quote, retweet,
  like, and bookmark

It does not implement DMs, notifications, or paid features.

## Requirements

- GNU Emacs 29.1 or newer
- `twitter-cli` installed and available as `twitter` or `twitter-cli`

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
M-x chirp-search
M-x chirp-thread
M-x chirp-profile
```

## Keys

- `g`: refresh; on Home and Following, the previous timeline snapshot stays available via `b`
- `b`: go back to the previous Chirp view
- `TAB`: switch between Home and Following when you are on either timeline
- `n` / `p`: next or previous entry; on Home and Following, `n` on the last entry loads more older posts
- `N`: load more older posts on Home and Following
- `RET`: open the current tweet or profile, or open large media when point is on a thumbnail
- `m`: open the first media item for the current tweet
- `A`: open the author profile
- `x`: open the actions menu for For You/Following, post/reply/quote, and tweet actions
- `o`: open the current item in a browser
- `q`: quit the window, or close the current media view and return

Inside the compose buffer:

- `C-c C-a`: attach an image file (up to 4)
- `C-c C-v`: paste one image from the clipboard
- `C-c C-d`: remove an attached image
- `C-c C-c`: close the draft immediately and send it in the background
- `C-c C-k` or `q`: cancel the draft

Tweet metrics also reflect the current local state: liked tweets show `Liked`,
bookmarked tweets show `Saved`, and retweeted tweets show `RTed`.
Clipboard image paste uses `wl-paste` on Wayland and `pngpaste` on macOS when available.

## Media

- Images render as small thumbnails in timeline, thread, and profile post lists.
- Thumbnails are rendered without inserted gaps, so point can move between adjacent images directly.
- Timeline, thread, and profile views now render cached avatars/thumbnails first; missing media are prefetched in the background so text appears faster.
- Video and animated GIF thumbnails are filled in asynchronously when Chirp can use an upstream preview image or extract one with `ffmpeg`.
- Press `RET` on a thumbnail to open the photo in `image-mode` in the same Chirp buffer when image display is available.
- In image and fallback media views, `q` returns to the previous Chirp view.
- Videos currently open externally through `mpv` when available, or the browser otherwise.

If you prefer the old blocking behavior, customize:

```elisp
(setq chirp-media-render-from-cache-only nil)
```

To disable background image prefetch, customize:

```elisp
(setq chirp-media-prefetch-images nil)
```

## Notes

- The package expects `twitter-cli --json` to return the documented envelope from `SCHEMA.md`.
- The parser is deliberately defensive because upstream X payloads can drift.
- Timeline "load more" currently re-fetches with a larger `--max`; `twitter-cli feed` does not expose cursor-based pagination yet.
- This repository was bootstrapped without `twitter-cli` installed locally, so the code is byte-compile checked but not end-to-end runtime tested yet.
