#!/bin/sh
set -eu

commit=0c21b0035888de99dfbcf4ca8304f566d906d794
short_commit=0c21b003
data_home=${XDG_DATA_HOME:-}
if [ -z "$data_home" ]; then
    home=${HOME:-}
    if [ -z "$home" ]; then
        printf '%s\n' 'HOME and XDG_DATA_HOME are both empty' >&2
        exit 1
    fi
    data_home=$home/.local/share
fi
prefix="$data_home/kristal-emacs-config/fennel-ls/$short_commit"
docset_dir="$data_home/fennel-ls/docsets"
script_dir=$(CDPATH= cd "$(dirname "$0")/.." && pwd -P)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT HUP INT TERM

git clone https://git.sr.ht/~xerool/fennel-ls "$tmp/fennel-ls"
git -C "$tmp/fennel-ls" checkout "$commit"
test "$(git -C "$tmp/fennel-ls" rev-parse HEAD)" = "$commit"
make -C "$tmp/fennel-ls" LUA=luajit
rm -rf "$prefix.new"
mkdir -p "$prefix.new"
make -C "$tmp/fennel-ls" install PREFIX="$prefix.new"
rm -rf "$prefix"
mv "$prefix.new" "$prefix"
mkdir -p "$docset_dir"
install -m 0644 "$script_dir/docsets/kristal.lua" "$docset_dir/kristal.lua"
printf 'installed fennel-ls %s at %s/bin/fennel-ls\n' "$commit" "$prefix"
