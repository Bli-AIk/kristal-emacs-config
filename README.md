# kristal-emacs-config

Project-local Emacs support for Kristal mods.

The repository is intended to be installed as a Git submodule at
`<mod>/.emacs/`. It provides LuaLS configuration, Kristal API metadata paths,
Lua inlay hints, and a script for launching the current mod through Kristal.

## Install as a submodule

```sh
git -C /path/to/your-mod submodule add \
  https://github.com/Bli-AIk/kristal-emacs-config.git .emacs
git -C /path/to/your-mod submodule update --init --recursive
```

The parent Mod should contain a `mod.json`. Emacs configuration detects
`.emacs/luarc.json` and starts LuaLS with that file as its project
configuration.

## Requirements

- Emacs 30 or newer with Doom's Lua and Eglot modules enabled.
- `lua-language-server` 3.13 or newer. Type inlay hints are enabled by the
  configuration; newer LuaLS releases provide the best inference results.
- LÖVE and Kristal for running the Mod.

The default paths are:

- `~/Projects/LuaProjects/Kristal`
- `~/Projects/LuaProjects/kristal-lua-docs`

Set `KRISTAL_ROOT` to override the engine path. Set
`KRISTAL_LUA_DOCS_LIBRARY` or `KRISTAL_LUA_DOCS` to override the Lua API docs
path.

## LuaLS features

The configuration enables:

- LuaJIT runtime semantics and the Love2D library.
- Kristal source, generated Lua docs, and official `luadoc_meta` signatures as
  external libraries. This includes the generic `Class` constructor signature.
- Assignment type hints, function parameter type hints, and parameter names.
- Workspace diagnostics on change and four-space formatting.

Emacs 30 displays the hints through Eglot's `eglot-inlay-hints-mode`. The
configuration does not add type annotations to Mod source files; LuaLS infers
what it can from source code and EmmyLua annotations.

## Run the Mod

From the Mod root:

```sh
sh .emacs/run-kristal-terminal.sh
sh .emacs/run-kristal-terminal.sh --hold
```

When launched from kitty or xterm, the script opens a separate terminal. In a
GUI or non-terminal environment it runs LÖVE in the current process so Emacs
can show its output.

## Validation

```sh
sh -n .emacs/run-kristal-terminal.sh
lua-language-server --check=. --configpath=.emacs/luarc.json \
  --check_format=pretty
```

The `--check` command may report existing project diagnostics; its purpose here
is to confirm that LuaLS can load the workspace configuration and libraries.

## License

Licensed under either of:

- Apache License, Version 2.0 (`LICENSE-APACHE`)
- MIT License (`LICENSE-MIT`)
