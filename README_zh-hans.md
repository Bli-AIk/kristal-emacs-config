# kristal-emacs-config

给 Kristal Mod 使用的项目级 Emacs 配置。

本仓库设计为以 Git 子模块放在 `<mod>/.emacs/` 中，提供 LuaLS 配置、
Kristal API 类型信息路径、Lua 内联类型提示，以及启动当前 Mod 的脚本。

## 作为子模块安装

```sh
git -C /path/to/your-mod submodule add \
  https://github.com/Bli-AIk/kristal-emacs-config.git .emacs
git -C /path/to/your-mod submodule update --init --recursive
```

Mod 根目录应包含 `mod.json`。Emacs 检测到 `.emacs/luarc.json` 后，会使用
该文件启动 LuaLS。

## 依赖

- Emacs 30 或更新版本，并启用 Doom 的 Lua 和 Eglot 模块。
- `lua-language-server` 3.13 或更新版本。
- 运行 Mod 需要 LÖVE 和 Kristal。

默认路径为：

- `~/Projects/LuaProjects/Kristal`
- `~/Projects/LuaProjects/kristal-lua-docs`

可以通过 `KRISTAL_ROOT` 覆盖 Kristal 引擎路径，通过
`KRISTAL_LUA_DOCS_LIBRARY` 或 `KRISTAL_LUA_DOCS` 覆盖 Lua API 文档路径。

## LuaLS 功能

配置启用：

- LuaJIT 运行时和 Love2D API。
- Kristal 源码及生成的 Lua 文档。
- 赋值类型提示、函数参数类型提示和参数名提示。
- 修改时工作区诊断，以及四空格格式化。

Emacs 30 通过 Eglot 的 `eglot-inlay-hints-mode` 显示这些提示。配置不会
修改 Mod 源码；LuaLS 会根据源码和 EmmyLua 注解进行推导。

## 运行 Mod

在 Mod 根目录执行：

```sh
sh .emacs/run-kristal-terminal.sh
sh .emacs/run-kristal-terminal.sh --hold
```

在 kitty 或 xterm 中执行时会打开独立终端；在 Emacs GUI 等非终端环境中会
直接运行 LÖVE，输出可由 Emacs 进程查看。

## 验证

```sh
sh -n .emacs/run-kristal-terminal.sh
lua-language-server --check=. --configpath=.emacs/luarc.json \
  --check_format=pretty
```

`--check` 可能显示项目已有诊断；这里主要用于确认 LuaLS 能读取工作区配置
和 API 库。

## 许可证

本仓库可任选以下许可证使用：

- Apache License 2.0（`LICENSE-APACHE`）
- MIT License（`LICENSE-MIT`）
