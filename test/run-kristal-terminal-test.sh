#!/bin/sh
set -eu

repo=$(CDPATH='' cd "$(dirname "$0")/.." && pwd -P)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT HUP INT TERM

mod_root="$tmp/mod with spaces"
config_root="$mod_root/.emacs"
engine_root="$tmp/Kristal engine"
fake_bin="$tmp/bin"
mkdir -p "$config_root" "$engine_root" "$fake_bin"
cp "$repo/run-kristal-terminal.sh" "$config_root/run-kristal-terminal.sh"
touch "$engine_root/main.lua"

cat > "$mod_root/mod.json" <<'EOF'
{
  "id": "foreground-fixture"
}
EOF

cat > "$fake_bin/love" <<'EOF'
#!/bin/sh
set -eu
pwd -P > "$FUMOS_LAUNCH_TEST_CWD"
printf '%s\n' "$$" > "$FUMOS_LAUNCH_TEST_PID"
: > "$FUMOS_LAUNCH_TEST_ARGS"
for argument do
  printf '%s\n' "$argument" >> "$FUMOS_LAUNCH_TEST_ARGS"
done
EOF

cat > "$fake_bin/kitty" <<'EOF'
#!/bin/sh
set -eu
{
  printf '%s\n' kitty
  for argument do
    printf '<%s>\n' "$argument"
  done
} > "$FUMOS_LAUNCH_TEST_TERMINAL"
EOF

cat > "$fake_bin/xterm" <<'EOF'
#!/bin/sh
set -eu
{
  printf '%s\n' xterm
  for argument do
    printf '<%s>\n' "$argument"
  done
} > "$FUMOS_LAUNCH_TEST_TERMINAL"
EOF
chmod 0755 "$fake_bin/love" "$fake_bin/kitty" "$fake_bin/xterm"

launcher="$config_root/run-kristal-terminal.sh"
cwd_file="$tmp/love.cwd"
pid_file="$tmp/love.pid"
args_file="$tmp/love.args"
terminal_file="$tmp/terminal.called"

env PATH="$fake_bin:$PATH" \
  KRISTAL_ROOT="$engine_root" \
  KITTY_WINDOW_ID=fixture TERM=xterm-kitty XTERM_VERSION=fixture \
  FUMOS_LAUNCH_TEST_CWD="$cwd_file" \
  FUMOS_LAUNCH_TEST_PID="$pid_file" \
  FUMOS_LAUNCH_TEST_ARGS="$args_file" \
  FUMOS_LAUNCH_TEST_TERMINAL="$terminal_file" \
  sh "$launcher" --foreground &
launcher_pid=$!
wait "$launcher_pid"

test ! -e "$terminal_file"
test "$(cat "$cwd_file")" = "$(CDPATH='' cd "$engine_root" && pwd -P)"
test "$(cat "$pid_file")" = "$launcher_pid"
diff -u - "$args_file" <<EOF
$engine_root
--mod
foreground-fixture
--auto-mod-start
EOF

expect_usage_error() {
  output="$tmp/usage.out"
  set +e
  sh "$launcher" "$@" >"$output" 2>&1
  status=$?
  set -e
  test "$status" -eq 64
  grep -Fq ' [--hold | --foreground]' "$output"
}

expect_usage_error --foreground --hold
expect_usage_error --hold --foreground
expect_usage_error --foreground --foreground
expect_usage_error --unknown

# The two established terminal-launch forms retain their kitty behavior.
env PATH="$fake_bin:$PATH" KRISTAL_ROOT="$engine_root" \
  KITTY_WINDOW_ID=fixture TERM=xterm-kitty \
  FUMOS_LAUNCH_TEST_TERMINAL="$terminal_file" \
  sh "$launcher"
grep -Fxq kitty "$terminal_file"
grep -Fxq '<--detach>' "$terminal_file"
if grep -Fxq '<--hold>' "$terminal_file"; then
  printf '%s\n' 'launcher added --hold without being asked' >&2
  exit 1
fi

env PATH="$fake_bin:$PATH" KRISTAL_ROOT="$engine_root" \
  KITTY_WINDOW_ID=fixture TERM=xterm-kitty \
  FUMOS_LAUNCH_TEST_TERMINAL="$terminal_file" \
  sh "$launcher" --hold
grep -Fxq kitty "$terminal_file"
grep -Fxq '<--detach>' "$terminal_file"
grep -Fxq '<--hold>' "$terminal_file"

printf '%s\n' 'Kristal launcher tests passed'
