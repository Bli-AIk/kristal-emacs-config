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
    case $home in
        /*) ;;
        *) printf '%s\n' 'HOME must be an absolute path' >&2; exit 1 ;;
    esac
    data_home=$home/.local/share
fi
case $data_home in
    /*) ;;
    *) printf '%s\n' 'XDG_DATA_HOME must be an absolute path' >&2; exit 1 ;;
esac
install_root="$data_home/kristal-emacs-config/fennel-ls"
prefix="$install_root/$short_commit"
docset_dir="$data_home/fennel-ls/docsets"
script_dir=$(CDPATH='' cd "$(dirname "$0")/.." && pwd -P)
command -v flock >/dev/null 2>&1 || {
    printf '%s\n' 'flock is required on Linux' >&2
    exit 1
}
lock_file="$install_root/.install-$short_commit.lock"
mkdir -p "$install_root"
exec 9>"$lock_file"
if ! flock -n 9; then
    printf 'another fennel-ls install holds %s\n' "$lock_file" >&2
    exit 1
fi
tmp=
stage=
doc_tmp=
cleanup() {
    [ -z "$doc_tmp" ] || rm -f "$doc_tmp"
    [ -z "$stage" ] || rm -rf "$stage"
    [ -z "$tmp" ] || rm -rf "$tmp"
}
trap cleanup EXIT
trap 'exit 1' HUP INT TERM

publish_docset() {
    mkdir -p "$docset_dir"
    doc_tmp=$(mktemp "$docset_dir/.kristal.lua.XXXXXX")
    install -m 0644 "$script_dir/docsets/kristal.lua" "$doc_tmp"
    mv -f -T "$doc_tmp" "$docset_dir/kristal.lua"
    doc_tmp=
}

if [ -e "$prefix" ]; then
    if [ -d "$prefix" ] && [ -f "$prefix/bin/fennel-ls" ] &&
       [ -x "$prefix/bin/fennel-ls" ] &&
       [ -f "$prefix/SOURCE_COMMIT" ] &&
       [ "$(cat "$prefix/SOURCE_COMMIT")" = "$commit" ]; then
        publish_docset
        printf 'reused fennel-ls %s at %s/bin/fennel-ls\n' "$commit" "$prefix"
        exit 0
    fi
    printf 'existing fennel-ls prefix is incomplete or untrusted: %s\n' \
        "$prefix" >&2
    exit 1
fi

tmp=$(mktemp -d)
git clone https://git.sr.ht/~xerool/fennel-ls "$tmp/fennel-ls"
git -C "$tmp/fennel-ls" checkout "$commit"
test "$(git -C "$tmp/fennel-ls" rev-parse HEAD)" = "$commit"
make -C "$tmp/fennel-ls" LUA=luajit
stage=$(mktemp -d "$install_root/.${short_commit}.new.XXXXXX")
make -C "$tmp/fennel-ls" install PREFIX="$stage"
test -x "$stage/bin/fennel-ls"
printf '%s\n' "$commit" > "$stage/SOURCE_COMMIT"
test ! -e "$prefix"
mv -T "$stage" "$prefix"
stage=
publish_docset
printf 'installed fennel-ls %s at %s/bin/fennel-ls\n' "$commit" "$prefix"
