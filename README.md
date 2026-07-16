# kristal-emacs-config

Project-local Emacs support for Lua and Fennel Kristal mods. In addition to
the existing LuaLS setup, this repository provides the FUMOS FLY workflow:
attach Emacs to the Fennel REPL inside a running development-mode Kristal
process, evaluate and reload explicitly, inspect Lisp data, and navigate
errors back to the originating `.fnl` source.

FUMOS stands for **Fumos Updates Mod Objects with S-expressions**.

## Support boundary and requirements

The complete v0.1 stack is supported on **Linux x86_64 with glibc only**. The
server-side platform adapter and client-side descriptor discovery are kept as
porting boundaries, but Windows and macOS are not currently supported.

The editor setup requires:

- Emacs 30 or newer. Doom Emacs is optional; vanilla Emacs uses the standard
  bindings documented below.
- LÖVE, Kristal, and a mod that loads the FUMOS library.
- Fennel 1.6.1 and LuaJIT for the pinned `fennel-ls` build.
- `lua-language-server` 3.13 or newer for Lua source support.
- `git`, `make`, and Linux `flock` to run the `fennel-ls` installer.

The default Lua/Kristal paths are:

- `~/Projects/LuaProjects/Kristal`
- `~/Projects/LuaProjects/kristal-lua-docs`

Set `KRISTAL_ROOT` to override the engine path. Set
`KRISTAL_LUA_DOCS_LIBRARY` or `KRISTAL_LUA_DOCS` to override the Lua API docs
path.

## Install as SSH submodules

Install this repository and the FUMOS library from the mod root. The teaching
project is specified to use these exact SSH URLs:

```sh
git -C /path/to/your-mod submodule add \
  git@github.com:Bli-AIk/kristal-emacs-config.git .emacs
git -C /path/to/your-mod submodule add \
  git@github.com:Bli-AIk/fumos.git libraries/fumos
git -C /path/to/your-mod submodule update --init --recursive
```

The parent mod must contain `mod.json`; FUMOS project detection additionally
requires `libraries/fumos/lib.json` and `.emacs/init.el`. Load
`.emacs/init.el` as the project entry before selecting the major mode. The
configured Doom setup does this through its generic project-local loader.

### Pinned Fennel tools

`vendor/fennel-mode/` is an unmodified, checksummed snapshot of
[fennel-mode](https://git.sr.ht/~technomancy/fennel-mode) at commit
`bbc28a629405de628880d8fb485fce23ff7fab69`. It includes
fennel-proto-repl 0.6.4. Verify the snapshot with:

```sh
(cd .emacs/vendor/fennel-mode && sha256sum -c SHA256SUMS)
```

The project entry fails closed if an already loaded fennel-mode or proto REPL
does not match the pinned source. After changing an Emacs package recipe, run
`doom sync` and fully restart Emacs rather than trying to replace a loaded
feature in place.

Install the pinned `fennel-ls` commit
`0c21b0035888de99dfbcf4ca8304f566d906d794` with:

```sh
sh .emacs/tools/install-fennel-ls.sh
```

The installer publishes a versioned executable under `XDG_DATA_HOME`, or
`~/.local/share` when that variable is unset or empty. In Emacs, install the
project template explicitly:

```text
M-x fumos-install-project-config
```

This command creates `flsproject.fnl` at the project root and refuses to
overwrite an existing file. Start Eglot normally after the template and the
pinned server are installed.

## LuaLS support

The existing LuaLS configuration enables:

- LuaJIT runtime semantics and the Love2D library.
- Kristal source, generated Lua docs, and official `luadoc_meta` signatures as
  external libraries, including the generic `Class` constructor signature.
- Assignment type hints, function parameter type hints, and parameter names.
- Workspace diagnostics on change and four-space formatting.

Emacs 30 displays the hints through `eglot-inlay-hints-mode`. The configuration
does not add annotations to mod source files; LuaLS infers types from source
and EmmyLua annotations.

## Start Kristal in development mode

From the mod root:

```sh
sh .emacs/run-kristal-terminal.sh
sh .emacs/run-kristal-terminal.sh --hold
```

When launched from kitty or xterm, the script opens a separate terminal. In a
GUI or non-terminal environment it runs LÖVE in the current process so Emacs
can show its output.

The in-game FUMOS service is automatically available only when
`Mod.info.dev == true`. It binds a random port on `127.0.0.1`; it is not a
remote REPL. On Linux, instance descriptors are stored in the non-empty
`$XDG_RUNTIME_DIR/fumos/` directory, falling back to `/tmp/fumos-$UID/` when
`XDG_RUNTIME_DIR` is unset or empty. The directory must be owned by the current
user with mode `0700`, and descriptor files must be owned by the current user
with mode `0600`.

Descriptors are validated for their exact schema, project root, live
same-user PID, loopback host, protocol, and capabilities before use. Their
256-bit token is used only for AUTH. It is never included in instance labels,
error conditions, `*Messages*`, or logs. Invalid descriptors expose only a
fixed field or error code.

## FLY attach and Lisp workflow

This is a live attach to the Fennel runtime inside the game, not a second
stdin-based Fennel REPL:

- `M-x fumos-connect` uses the current project's sole instance, asking for a
  choice when needed.
- `M-x fumos-attach` is the explicit instance-selection operation.
- `M-x fumos-connect-or-switch` connects when needed and otherwise displays
  the attached game REPL, whether it is ready or currently busy.
- `M-x fumos-reconnect` reconnects only to the previously attached PID. Use
  `fumos-attach` to choose a different process.

A typical Doom session is:

```text
M-x fumos-connect
SPC m e e
SPC m c c
SPC m r i
```

The interaction model follows Common Lisp/SLY where it fits Fennel: operations
are explicit, evaluated forms share persistent lexical REPL locals during one
attachment, macros can be expanded and refreshed, and multiple return values
remain distinct in the result UI. Evaluation requests carry authenticated
source context, so compile and runtime errors can navigate to the originating
line and column.

Saving a file **never** evaluates, compiles, reloads, or otherwise changes the
game. `fumos-reload-current-file` requires an explicitly saved, unmodified
source file. Compile commands use the current in-memory text without saving or
executing it. A reconnect creates a fresh lexical REPL session and loses REPL
locals, while game state and already committed live definitions remain.

`C-c C-c` interrupts the active request. FUMOS v0.1 does not provide Common
Lisp conditions/restarts or resume an interrupted stack frame.

### Standard Emacs bindings

These bindings exist only while `fumos-mode` or `fumos-repl-mode` owns the
current buffer:

| Key | Command | Action |
| --- | --- | --- |
| `C-x C-e` | `fumos-eval-last-sexp` | Evaluate the expression before point |
| `C-M-x` | `fumos-eval-defun` | Evaluate the current top-level form |
| `C-c C-r` | `fumos-eval-region` | Evaluate the active region |
| `C-c C-b` | `fumos-eval-buffer` | Evaluate the widened buffer |
| `C-c C-k` | `fumos-reload-current-file` | Semantically reload the saved current source |
| `C-c C-z` | `fumos-switch-to-repl` | Display the attached game REPL |
| REPL `C-c C-c` | `fumos-interrupt` | Interrupt the current request through FUMOS |

### Doom localleader bindings

The source-buffer menu contains exactly 33 bindings. Normal, visual, and
motion states use `SPC m`; insert and Emacs states use `M-SPC m`. In the table,
replace the displayed `SPC m` prefix with `M-SPC m` in those latter two states.
The `c`, `e`, `g`, `h`, and `r` prefixes are respectively described by
which-key as `compile/reload`, `evaluate`, `goto`, `help`, and `repl`.

| Key | Command | Action |
| --- | --- | --- |
| `SPC m '` | `fumos-connect-or-switch` | Connect or switch to the game REPL |
| `SPC m ;` | `fumos-attach` | Explicitly select and attach an instance |
| `SPC m m` | `fumos-macroexpand` | Expand the form at point |
| `SPC m c c` | `fumos-reload-current-file` | Reload the current saved file |
| `SPC m c m` | `fumos-reload-module` | Reload a named Fennel module |
| `SPC m c f` | `fumos-compile-defun` | Compile the current top-level form |
| `SPC m c b` | `fumos-compile-buffer` | Compile the in-memory buffer |
| `SPC m e b` | `fumos-eval-buffer` | Evaluate the buffer |
| `SPC m e d` | `fumos-eval-defun-overlay` | Evaluate the form with a result overlay |
| `SPC m e e` | `fumos-eval-last-sexp` | Evaluate the previous expression |
| `SPC m e E` | `fumos-eval-print-last-sexp` | Evaluate and insert all returned values |
| `SPC m e f` | `fumos-eval-defun-async` | Queue the top-level form for echo-area output |
| `SPC m e n` | `fumos-eval-form-and-next` | Evaluate the form and advance |
| `SPC m e r` | `fumos-eval-region` | Evaluate the region |
| `SPC m g b` | `xref-go-back` | Return to the previous xref location |
| `SPC m g d` | `fumos-find-definition` | Find a runtime definition |
| `SPC m g D` | `fumos-find-definition-other-window` | Find a definition in another window |
| `SPC m g n` | `fumos-next-error` | Visit the next owned FUMOS error |
| `SPC m g N` | `fumos-previous-error` | Visit the previous owned FUMOS error |
| `SPC m h a` | `fumos-apropos` | Search runtime symbols |
| `SPC m h h` | `fumos-show-documentation` | Show symbol documentation in the game REPL |
| `SPC m h A` | `fumos-show-arglist` | Show a symbol's argument list |
| `SPC m h m` | `fumos-macroexpand` | Expand the form at point |
| `SPC m h l` | `fumos-show-generated-lua` | Show the last generated Lua |
| `SPC m r a` | `fumos-attach` | Explicitly select and attach an instance |
| `SPC m r c` | `fumos-clear-repl` | Clear the current game REPL |
| `SPC m r i` | `fumos-interrupt` | Interrupt the active evaluation |
| `SPC m r q` | `fumos-disconnect` | Detach without stopping Kristal |
| `SPC m r r` | `fumos-reconnect` | Reconnect to the same PID |
| `SPC m r s` | `fumos-switch-to-repl` | Display the game REPL |
| `SPC m r R` | `fumos-reload-game-preserve` | Reload while preserving temporary state |
| `SPC m r L` | `fumos-reload-game-save` | Reload from the latest save |
| `SPC m r 0` | `fumos-reload-game-from-start` | Reload from the beginning |

These 33 bindings are installed only in `fumos-mode` source buffers. Ordinary
Fennel projects retain their existing localleader bindings. The one additional
REPL-context binding, `SPC m r i` (or `M-SPC m r i`), is installed only in
FUMOS's own `fumos-repl-mode-map`; an ordinary `fennel-proto-repl-mode` buffer
retains the exact binding it had before FUMOS loaded.

### Game reload modes

| State source | Wire mode | Command | Doom key |
| --- | --- | --- | --- |
| Preserve temporary state | `temp` | `fumos-reload-game-preserve` | `SPC m r R` |
| Latest save | `save` | `fumos-reload-game-save` | `SPC m r L` |
| Beginning | `none` | `fumos-reload-game-from-start` | `SPC m r 0` |

Each command asks FUMOS to schedule a Kristal reload, watches for a replacement
descriptor belonging to the same PID, and attaches to the new REPL session.
Synchronous or asynchronous errors, `C-g`/`quit`, and other nonlocal exits
cancel that operation's descriptor polling. Only the expected
`connection-lost` transition lets the same operation generation keep waiting
for the replacement.

### Connection and buffer isolation

FUMOS uses the reserved bootstrap module `fumos.repl.fennel`. The corresponding
upstream module-name variable is buffer-local in each game REPL; the user's
global Fennel REPL default is never changed.

When several accepted requests are active, the mode line remains busy.
`fumos-cancel-active-request` asks for a request ID when more than one is
active, and the connection returns to ready only after the final `done`.

On disconnect, undelivered ordinary callbacks are cancelled. Every request
that has not actually received a values/error terminal receives at most one
`connection-lost` terminal. Seeing `done` is not treated as proof that a
terminal was delivered, and timers from an old same-PID connection cannot
update the replacement connection's UI.

FUMOS never takes over a user buffer that already has its deterministic REPL
name. A collision rejects the connection without changing that buffer's
contents, major mode, name, or liveness. Source links have a buffer-local
owner and preserve the exact target and proto-mode state from before the first
ordinary-REPL-to-FUMOS link. Relinking A to B carries that snapshot; clear,
transport teardown, and buffer kill restore the same live ordinary target and
its prior mode. Explicitly disabling proto mode restores the target while
preserving the new disabled intent, and a later ordinary relink takes
precedence. Every cleanup path immediately releases only old FUMOS links and
xref ownership while retaining unrelated xref backends.

## Teaching branches and compatibility targets

The downstream `fumos-test` repository supplies two teaching branches:

- `main` is a playable mixed Lua/Fennel project with Fennel bullets, dialogue,
  and integration with existing Lua.
- `pure-fennel` is the corresponding template branch without mod-author Lua.

They are downstream release artifacts, not fixtures in this repository. The
exact Kristal compatibility targets are:

- `752bc0688ba97ca8a256ba9125b7e05a1ca6edbd` for the 0.10 baseline.
- `8ee0129cbd18daa5334eb458096da9e0ee484ad5` for the 0.11 development baseline.

These SHAs are release targets. Real game/Emacs end-to-end verification belongs
to the `fumos-test` release process and must not be inferred from this
repository's editor test suites.

## Validation

Run the five validation entry points from `.emacs/` or this repository's root:

```sh
make test
make test-upstream
make test-doom
make test-installer
make testall
```

`make test` is the vanilla Emacs suite, `make test-upstream` verifies the
vendored upstream snapshot, `make test-doom` uses the real isolated Doom PTY
profile, and `make test-installer` exercises the pinned installer. `make
testall` runs all four. Test counts are intentionally not frozen; read the
current ERT counts from the output and require `0 unexpected`.

For the legacy LuaLS configuration, this remains a useful workspace check:

```sh
lua-language-server --check=. --configpath=.emacs/luarc.json \
  --check_format=pretty
```

It may report existing project diagnostics; its purpose is to confirm that
LuaLS can load the workspace configuration and libraries.

## License

This is a mixed-license repository:

- FUMOS Elisp, Elisp tests, the project entry, and the vendored fennel-mode
  snapshot are GPL-3.0-or-later. See `LICENSE-GPL-3.0-or-later` and
  `THIRD_PARTY.md`.
- Existing LuaLS data and the launcher remain available under the repository's
  MIT/Apache dual-license terms. See `LICENSE-MIT` and `LICENSE-APACHE`.

The repository as a whole must not be described as entirely dual-licensed or
entirely GPL. Third-party source retains its own upstream license and provenance.
