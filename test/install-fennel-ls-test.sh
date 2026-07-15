#!/bin/sh
set -eu

repo=$(CDPATH= cd "$(dirname "$0")/.." && pwd -P)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT HUP INT TERM
fake_bin="$tmp/bin"
mkdir -p "$fake_bin"

cat > "$fake_bin/git" <<'EOF'
#!/bin/sh
set -eu
case $1 in
    clone) mkdir -p "$3" ;;
    -C)
        case $3 in
            checkout) ;;
            rev-parse) printf '%s\n' 0c21b0035888de99dfbcf4ca8304f566d906d794 ;;
            *) exit 2 ;;
        esac
        ;;
    *) exit 2 ;;
esac
EOF

cat > "$fake_bin/make" <<'EOF'
#!/bin/sh
set -eu
prefix=
for argument do
    case $argument in PREFIX=*) prefix=${argument#PREFIX=} ;; esac
done
if [ -n "$prefix" ]; then
    [ "${FUMOS_FAKE_INSTALL_FAIL:-0}" != 1 ] || exit 42
    mkdir -p "$prefix/bin"
    printf '%s\n' '#!/bin/sh' 'exit 0' > "$prefix/bin/fennel-ls"
    chmod 0755 "$prefix/bin/fennel-ls"
fi
EOF
chmod 0755 "$fake_bin/git" "$fake_bin/make"

run_install() {
    env PATH="$fake_bin:$PATH" XDG_DATA_HOME="$1" HOME=/nonexistent \
        sh "$repo/tools/install-fennel-ls.sh"
}

data="$tmp/data"
run_install "$data" >/dev/null
prefix="$data/kristal-emacs-config/fennel-ls/0c21b003"
test -x "$prefix/bin/fennel-ls"
test "$(cat "$prefix/SOURCE_COMMIT")" = \
    0c21b0035888de99dfbcf4ca8304f566d906d794
test -f "$data/fennel-ls/docsets/kristal.lua"
inode=$(stat -c %i "$prefix")
run_install "$data" | rg -q '^reused fennel-ls '
test "$(stat -c %i "$prefix")" = "$inode"

lock="$data/kristal-emacs-config/fennel-ls/.install-0c21b003.lock"
exec 8>"$lock"
flock -n 8
if run_install "$data" >/dev/null 2>&1; then
    printf '%s\n' 'installer ignored an active lock' >&2
    exit 1
fi
test "$(stat -c %i "$prefix")" = "$inode"
flock -u 8
exec 8>&-

incomplete="$tmp/incomplete"
bad_prefix="$incomplete/kristal-emacs-config/fennel-ls/0c21b003"
mkdir -p "$bad_prefix"
touch "$bad_prefix/keep"
if run_install "$incomplete" >/dev/null 2>&1; then
    printf '%s\n' 'installer accepted an incomplete prefix' >&2
    exit 1
fi
test -f "$bad_prefix/keep"

directory_binary="$tmp/directory-binary"
directory_prefix="$directory_binary/kristal-emacs-config/fennel-ls/0c21b003"
mkdir -p "$directory_prefix/bin/fennel-ls"
printf '%s\n' 0c21b0035888de99dfbcf4ca8304f566d906d794 > \
    "$directory_prefix/SOURCE_COMMIT"
if run_install "$directory_binary" >/dev/null 2>&1; then
    printf '%s\n' 'installer accepted a directory as the executable' >&2
    exit 1
fi
test -d "$directory_prefix/bin/fennel-ls"

failed="$tmp/failed"
if env FUMOS_FAKE_INSTALL_FAIL=1 PATH="$fake_bin:$PATH" \
    XDG_DATA_HOME="$failed" HOME=/nonexistent \
    sh "$repo/tools/install-fennel-ls.sh" >/dev/null 2>&1; then
    printf '%s\n' 'installer ignored a failed staged install' >&2
    exit 1
fi
test ! -e "$failed/kristal-emacs-config/fennel-ls/0c21b003"
test -z "$(find "$failed" -name '.0c21b003.new.*' -print -quit)"
test -f "$failed/kristal-emacs-config/fennel-ls/.install-0c21b003.lock"

printf '%s\n' 'FUMOS fennel-ls installer tests passed'
