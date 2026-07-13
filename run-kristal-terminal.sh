#!/bin/sh
set -eu

hold=0
case "${1:-}" in
  --hold)
    hold=1
    ;;
  "")
    ;;
  *)
    echo "usage: $0 [--hold]" >&2
    exit 64
    ;;
esac

script_path=$0
case "$script_path" in
  */*) ;;
  *)
    script_path=$(command -v "$script_path") || {
      echo "cannot locate script: $0" >&2
      exit 1
    }
    ;;
esac

script_dir=$(CDPATH= cd "$(dirname "$script_path")" && pwd -P)
mod_root=$(CDPATH= cd "$script_dir/.." && pwd -P)

mod_id=""
if [ -f "$mod_root/mod.json" ]; then
  mod_id=$(sed -n 's/^[[:space:]]*"id"[[:space:]]*:[[:space:]]*"\([^"]*\)".*$/\1/p' "$mod_root/mod.json" | head -n 1)
fi
if [ -z "$mod_id" ]; then
  mod_id=$(basename "$mod_root")
fi

find_engine_root() {
  if [ -n "${KRISTAL_ROOT:-}" ]; then
    printf '%s\n' "$KRISTAL_ROOT"
    return 0
  fi

  for candidate in \
    "$mod_root/../Kristal" \
    "$mod_root/../kristal" \
    "$mod_root/../../Kristal" \
    "$mod_root/../../kristal" \
    "$HOME/Projects/LuaProjects/Kristal" \
    "$HOME/Projects/Kristal" \
    "$HOME/Kristal"
  do
    if [ -f "$candidate/main.lua" ]; then
      CDPATH= cd "$candidate" && pwd -P
      return 0
    fi
  done

  return 1
}

engine_root=$(find_engine_root) || {
  echo "Kristal engine not found. Set KRISTAL_ROOT=/path/to/Kristal." >&2
  exit 1
}

if [ ! -f "$engine_root/main.lua" ]; then
  echo "Kristal engine main.lua not found: $engine_root/main.lua" >&2
  exit 1
fi

run_game() {
  cd "$engine_root"
  exec love "$engine_root" --mod "$mod_id" --auto-mod-start
}

title="Kristal - $mod_id"

terminal=""
if [ -n "${KITTY_WINDOW_ID:-}" ] || [ "${TERM:-}" = "xterm-kitty" ]; then
  terminal="kitty"
elif [ -n "${XTERM_VERSION:-}" ]; then
  terminal="xterm"
else
  case "${TERM:-}" in
    xterm|xterm-*)
      terminal="xterm"
      ;;
  esac
fi

case "$terminal" in
  kitty)
    if [ "$hold" -eq 1 ]; then
      kitty --detach --hold --title "$title" --directory "$engine_root" \
        sh -c 'love "$1" --mod "$2" --auto-mod-start; status=$?; printf "\nLove exited with status %s\n" "$status"; exec "${SHELL:-sh}" -i' \
        sh "$engine_root" "$mod_id" >/dev/null 2>&1
    else
      kitty --detach --title "$title" --directory "$engine_root" \
        love "$engine_root" --mod "$mod_id" --auto-mod-start >/dev/null 2>&1
    fi
    ;;
  xterm)
    if [ "$hold" -eq 1 ]; then
      xterm -hold -T "$title" -e sh -c \
        'love "$1" --mod "$2" --auto-mod-start; status=$?; printf "\nLove exited with status %s\n" "$status"; exec "${SHELL:-sh}" -i' \
        sh "$engine_root" "$mod_id" >/dev/null 2>&1 &
    else
      xterm -T "$title" -e love "$engine_root" --mod "$mod_id" --auto-mod-start \
        >/dev/null 2>&1 &
    fi
    ;;
  *)
    if [ "$hold" -eq 1 ]; then
      set +e
      cd "$engine_root"
      love "$engine_root" --mod "$mod_id" --auto-mod-start
      status=$?
      printf '\nLove exited with status %s\n' "$status"
      exit "$status"
    fi
    run_game
    ;;
esac
