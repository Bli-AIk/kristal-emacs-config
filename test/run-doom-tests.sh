#!/bin/sh
# SPDX-License-Identifier: GPL-3.0-or-later
set -eu

umask 077
unset CDPATH
repo=$(cd "$(dirname "$0")/.." && pwd -P)
emacsdir=${EMACSDIR:-"$HOME/.config/emacs"}
doomdir=${DOOMDIR:-"$HOME/.config/doom"}
emacsdir=$(cd "$emacsdir" && pwd -P)
doomdir=$(cd "$doomdir" && pwd -P)

test -f "$emacsdir/early-init.el"
test -f "$doomdir/config.el"
test -d "$emacsdir/.local/cache"
test -d "$emacsdir/.local/state"
test -d "$emacsdir/.local/etc/workspaces"

bwrap_bin=$(command -v bwrap)
emacs_bin=$(command -v "${EMACS:-emacs}")
script_bin=$(command -v script)
timeout_bin=$(command -v timeout)
command -v pgrep >/dev/null 2>&1

timeout_seconds=${FUMOS_DOOM_TEST_TIMEOUT:-90}
case $timeout_seconds in
  ''|*[!0-9]*|0)
    echo "FUMOS_DOOM_TEST_TIMEOUT must be a positive integer" >&2
    exit 2
    ;;
esac

case ${TERM:-} in
  ''|dumb|unknown) term=xterm-256color ;;
  *) term=$TERM ;;
esac

sandbox=$(mktemp -d "${TMPDIR:-/tmp}/fumos-doom.XXXXXXXX")
process_tag="fumos-doom-test-$$"
trap 'rm -rf -- "$sandbox"' EXIT
trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM

mkdir -p \
  "$sandbox/tmp/home/.config/emacs" \
  "$sandbox/tmp/home/.config/doom" \
  "$sandbox/tmp/xdg/cache" \
  "$sandbox/tmp/xdg/data" \
  "$sandbox/tmp/xdg/runtime" \
  "$sandbox/tmp/xdg/state" \
  "$sandbox/cache" \
  "$sandbox/state" \
  "$sandbox/workspaces"

cat >"$sandbox/tmp/fumos-doom-child.sh" <<'CHILD'
#!/bin/sh
# SPDX-License-Identifier: GPL-3.0-or-later
set -eu

# Prove that the vanilla entry does not load Doom-only integration first.
"$FUMOS_DOOM_EMACS" -Q --batch \
  -L "$FUMOS_DOOM_REPO/vendor/fennel-mode" \
  -L "$FUMOS_DOOM_REPO/lisp" \
  -l "$FUMOS_DOOM_REPO/init.el" \
  --eval '(unless (featurep (quote kristal-emacs-config)) (kill-emacs 2))' \
  --eval '(when (or (featurep (quote fumos-doom))
                     (fboundp (quote fumos-doom-install)))
             (kill-emacs 2))'

# A PTY keeps noninteractive nil; ERT starts from the normal startup hook.
exec "$FUMOS_DOOM_EMACS" \
  --no-window-system \
  --name "$FUMOS_DOOM_PROCESS_TAG" \
  --init-directory "$FUMOS_DOOM_EMACSDIR" \
  -L "$FUMOS_DOOM_REPO/vendor/fennel-mode" \
  -L "$FUMOS_DOOM_REPO/lisp" \
  -L "$FUMOS_DOOM_REPO/test" \
  -l "$FUMOS_DOOM_REPO/test/fumos-doom-test.el"
CHILD
chmod 0700 "$sandbox/tmp/fumos-doom-child.sh"

status=0
"$timeout_bin" --kill-after=5s "${timeout_seconds}s" \
  "$bwrap_bin" \
    --die-with-parent \
    --unshare-all \
    --new-session \
    --cap-drop ALL \
    --ro-bind / / \
    --dev /dev \
    --proc /proc \
    --bind "$sandbox/tmp" /tmp \
    --ro-bind "$emacsdir" /tmp/home/.config/emacs \
    --ro-bind "$doomdir" /tmp/home/.config/doom \
    --bind "$sandbox/cache" "$emacsdir/.local/cache" \
    --bind "$sandbox/state" "$emacsdir/.local/state" \
    --bind "$sandbox/workspaces" "$emacsdir/.local/etc/workspaces" \
    --clearenv \
    --setenv PATH "$PATH" \
    --setenv HOME /tmp/home \
    --setenv LANG C.UTF-8 \
    --setenv LC_ALL C.UTF-8 \
    --setenv TERM "$term" \
    --setenv EMACSDIR "$emacsdir" \
    --setenv DOOMDIR "$doomdir" \
    --setenv XDG_CACHE_HOME /tmp/xdg/cache \
    --setenv XDG_DATA_HOME /tmp/xdg/data \
    --setenv XDG_RUNTIME_DIR /tmp/xdg/runtime \
    --setenv XDG_STATE_HOME /tmp/xdg/state \
    --setenv FUMOS_DOOM_EMACS "$emacs_bin" \
    --setenv FUMOS_DOOM_EMACSDIR "$emacsdir" \
    --setenv FUMOS_DOOM_PROCESS_TAG "$process_tag" \
    --setenv FUMOS_DOOM_REPO "$repo" \
    --chdir "$repo" \
    "$script_bin" -qefc /tmp/fumos-doom-child.sh /dev/null || status=$?

# The PID namespace, die-with-parent, and kill-after should clear descendants.
if pgrep -f -- "$process_tag" >/dev/null 2>&1; then
  echo "FUMOS Doom test left a process behind: $process_tag" >&2
  exit 125
fi

case $status in
  0) ;;
  124|137)
    echo "FUMOS Doom test timed out after ${timeout_seconds}s" >&2
    ;;
  *)
    echo "FUMOS Doom test failed with status $status" >&2
    ;;
esac
exit "$status"
