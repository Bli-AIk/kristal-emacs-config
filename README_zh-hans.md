# kristal-emacs-config

面向 Lua/Fennel Kristal Mod 的项目级 Emacs 支持。除原有 LuaLS 配置外，本仓库
还提供 FUMOS FLY 工作流：让 Emacs 附着到以开发模式运行的 Kristal 进程内 Fennel
REPL，显式求值和重载、检查 Lisp 数据，并把错误导航回原始 `.fnl` 源码。

FUMOS 全称为 **Fumos Updates Mod Objects with S-expressions**。

## 分支

本仓库维护三条有明确用途的配置线：

- \`main\`：共享 Lua/Kristal 支持和可选 FUMOS 工具的当前集成线。
- \`stable/lua\`：标准 Kristal 模板使用的最小 Lua 基线。
- \`experimental/fumos\`：FUMOS FLY 工作流、Fennel 工具和游戏内 live REPL。

父 Mod 应显式固定所需分支，不要依赖未声明的子模块 checkout：

\`\`\`sh
git submodule set-branch --branch stable/lua .emacs
# 或者，在 FUMOS 项目中使用：
git submodule set-branch --branch experimental/fumos .emacs
\`\`\`

## 支持边界与依赖

v0.1 完整栈目前只正式支持 **Linux x86_64 + glibc**。服务端 platform adapter
与客户端 descriptor discovery 已被隔离为将来的移植边界，但 Windows 和 macOS
当前均不受支持。

编辑器环境需要：

- Emacs 30 或更新版本。Doom Emacs 是可选项；vanilla Emacs 可使用下文的标准键位。
- LÖVE、Kristal，以及已加载 FUMOS library 的 Mod。
- 用于构建固定 `fennel-ls` 的 Fennel 1.6.1 与 LuaJIT。
- 用于 Lua 源码支持的 `lua-language-server` 3.13 或更新版本。
- 用于运行 `fennel-ls` 安装器的 `git`、`make` 和 Linux `flock`。

Lua/Kristal 默认路径为：

- `~/Projects/LuaProjects/Kristal`
- `~/Projects/LuaProjects/kristal-lua-docs`

可以通过 `KRISTAL_ROOT` 覆盖引擎路径，通过 `KRISTAL_LUA_DOCS_LIBRARY` 或
`KRISTAL_LUA_DOCS` 覆盖 Lua API 文档路径。

## 通过 SSH 子模块安装

在 Mod 根目录安装本仓库和 FUMOS library。发布后的教学项目使用以下精确 SSH URL：

```sh
git -C /path/to/your-mod submodule add \
  git@github.com:Bli-AIk/kristal-emacs-config.git .emacs
git -C /path/to/your-mod submodule add \
  git@github.com:Bli-AIk/fumos.git libraries/fumos
git -C /path/to/your-mod submodule update --init --recursive
```

父 Mod 必须包含 `mod.json`；FUMOS 项目识别还要求存在
`libraries/fumos/lib.json` 和 `.emacs/init.el`。选择 major mode 前，应把
`.emacs/init.el` 作为项目入口载入。当前配置的 Doom 环境通过通用项目 loader
完成这一步。

### 固定版本的 Fennel 工具

`vendor/fennel-mode/` 是
[fennel-mode](https://git.sr.ht/~technomancy/fennel-mode) 提交
`bbc28a629405de628880d8fb485fce23ff7fab69` 的未修改、带校验和快照，其中包含
fennel-proto-repl 0.6.4。使用以下命令验证快照：

```sh
(cd .emacs/vendor/fennel-mode && sha256sum -c SHA256SUMS)
```

如果已经加载的 fennel-mode 或 proto REPL 与固定源码不符，项目入口会 fail
closed。修改 Emacs package recipe 后，应执行 `doom sync` 并完整重启 Emacs，
不能尝试就地替换已经加载的 feature。

使用以下命令安装固定到提交
`0c21b0035888de99dfbcf4ca8304f566d906d794` 的 `fennel-ls`：

```sh
sh .emacs/tools/install-fennel-ls.sh
```

安装器会把带版本的可执行文件发布到 `XDG_DATA_HOME`；该变量未设置或为空时使用
`~/.local/share`。随后在 Emacs 中显式安装项目模板：

```text
M-x fumos-install-project-config
```

此命令在项目根目录创建 `flsproject.fnl`，并拒绝覆盖已经存在的文件。安装模板和
固定 server 后，按通常方式启动 Eglot。

## LuaLS 支持

原有 LuaLS 配置启用：

- LuaJIT 运行时语义和 Love2D library。
- 作为外部 library 的 Kristal 源码、生成的 Lua 文档和官方 `luadoc_meta` 签名，
  包括泛型 `Class` 构造器签名。
- 赋值类型提示、函数参数类型提示和参数名提示。
- 修改时工作区诊断，以及四空格格式化。

Emacs 30 通过 `eglot-inlay-hints-mode` 显示提示。配置不会向 Mod 源码添加注解；
LuaLS 根据源码和 EmmyLua 注解进行类型推导。

## 以开发模式启动 Kristal

在 Mod 根目录执行：

```sh
sh .emacs/run-kristal-terminal.sh
sh .emacs/run-kristal-terminal.sh --hold
sh .emacs/run-kristal-terminal.sh --foreground
```

从 kitty 或 xterm 启动时，脚本会打开独立终端；在 GUI 或非终端环境中，它会在
当前进程运行 LÖVE，使 Emacs 可以显示输出。`--hold` 会在 LÖVE 退出后保留已打开的
终端。`--foreground` 始终绕过终端探测，并用 LÖVE 替换启动器进程，供 Emacs 等
进程管理方使用；`--hold` 与 `--foreground` 不能同时使用。

游戏内 FUMOS 服务只在 `Mod.info.dev == true` 时自动可用。它在 `127.0.0.1`
上绑定随机端口，不是远程 REPL。在 Linux 上，实例描述文件位于非空的
`$XDG_RUNTIME_DIR/fumos/`；`XDG_RUNTIME_DIR` 未设置或为空时回退到
`/tmp/fumos-$UID/`。目录必须归当前用户所有且权限为 `0700`，描述文件必须归当前
用户所有且权限为 `0600`。

使用描述文件前，客户端会验证精确 schema、项目根目录、仍存活且同用户的 PID、
loopback host、协议和 capabilities。256 位 token 只用于 AUTH，绝不进入实例选择
标签、错误 condition、`*Messages*` 或日志。无效描述文件只暴露固定 field 或错误码。

## FLY 附着与 Lisp 工作流

这条工作流会附着到游戏进程内的 Fennel runtime，不会另行启动一个基于 stdin 的
Fennel REPL：

- `M-x fumos-connect` 使用当前项目的唯一实例；需要时会要求选择。
- `M-x fumos-attach` 是显式选择实例的操作。
- `M-x fumos-connect-or-switch` 在需要时连接，否则显示已附着的游戏 REPL；该
  REPL 可以处于 ready 或 busy 状态。在 Doom 中，如果项目没有正在运行的实例，
  `SPC m '` 还会按需启动 Kristal，然后在同一次操作中完成连接并显示 REPL；启动后
  无需再次按键。
- `M-x fumos-reconnect` 只重新连接先前附着的 PID。要选择另一个进程，应使用
  `fumos-attach`。

典型 Doom 会话如下：

```text
SPC m '
SPC m e e
SPC m c c
SPC m r i
```

交互模型在适合 Fennel 的范围内对齐 Common Lisp/SLY：所有操作都由用户显式触发；
同一次附着中的求值 form 共享持久 lexical REPL locals；宏可以展开和刷新；多返回值
在结果 UI 中保持为独立值。求值请求携带经过认证的源码上下文，因此编译错误和运行时
错误可以导航到原始行列。

保存文件**绝不会**求值、编译、重载或以其他方式改变游戏。
`fumos-reload-current-file` 要求源码已经由用户显式保存，且 buffer 没有未保存修改。
编译命令使用当前内存文本，不保存也不执行。重新连接会创建新的 lexical REPL
session 并丢失 REPL locals，但游戏状态和已经提交的 live definition 会保留。

`C-c C-c` 用于中断当前请求。FUMOS v0.1 不提供 Common Lisp conditions/restarts，
也不能在中断后继续原栈帧。

### 标准 Emacs 键位

以下键位只在当前 buffer 由 `fumos-mode` 或 `fumos-repl-mode` 管理时生效：

| 键位 | 命令 | 操作 |
| --- | --- | --- |
| `C-x C-e` | `fumos-eval-last-sexp` | 求值 point 前的表达式 |
| `C-M-x` | `fumos-eval-defun` | 求值当前顶层 form |
| `C-c C-r` | `fumos-eval-region` | 求值选中 region |
| `C-c C-b` | `fumos-eval-buffer` | 求值 widen 后的 buffer |
| `C-c C-k` | `fumos-reload-current-file` | 语义化重载已保存的当前源码 |
| `C-c C-z` | `fumos-switch-to-repl` | 显示已附着的游戏 REPL |
| REPL `C-c C-c` | `fumos-interrupt` | 通过 FUMOS 中断当前请求 |

### Doom localleader 键位

源码 buffer 菜单精确包含 36 个键位。normal、visual、motion state 使用
`SPC m`；insert 和 emacs state 使用 `M-SPC m`。在后两种 state 中，应把下表显示的
`SPC m` 前缀替换为 `M-SPC m`。which-key 分别把 `c`、`e`、`g`、`h`、`r`
前缀显示为 `compile/reload`、`evaluate`、`goto`、`help` 和 `repl`。

| 键位 | 命令 | 操作 |
| --- | --- | --- |
| `SPC m '` | `fumos-connect-or-switch` | 按需启动、连接并显示游戏 REPL |
| `SPC m ;` | `fumos-attach` | 显式选择并附着实例 |
| `SPC m m` | `fumos-macroexpand` | 展开 point 处的 form |
| `SPC m R` | `fumos-reload-game-preserve` | Kristal `Ctrl+R`：保留临时状态并快速重载 |
| `SPC m S` | `fumos-reload-game-save` | Kristal `Ctrl+Shift+R`：从最近存档重载 |
| `SPC m 0` | `fumos-reload-game-from-start` | Kristal `Ctrl+Alt+R`：从头开始重载 |
| `SPC m c c` | `fumos-reload-current-file` | 重载已保存的当前文件 |
| `SPC m c m` | `fumos-reload-module` | 重载指定名称的 Fennel module |
| `SPC m c f` | `fumos-compile-defun` | 编译当前顶层 form |
| `SPC m c b` | `fumos-compile-buffer` | 编译内存中的 buffer |
| `SPC m e b` | `fumos-eval-buffer` | 求值 buffer |
| `SPC m e d` | `fumos-eval-defun-overlay` | 求值 form 并显示结果 overlay |
| `SPC m e e` | `fumos-eval-last-sexp` | 求值前一个表达式 |
| `SPC m e E` | `fumos-eval-print-last-sexp` | 求值并插入全部返回值 |
| `SPC m e f` | `fumos-eval-defun-async` | 将顶层 form 加入 echo-area 输出队列 |
| `SPC m e n` | `fumos-eval-form-and-next` | 求值当前 form 并移动到下一个 |
| `SPC m e r` | `fumos-eval-region` | 求值 region |
| `SPC m g b` | `xref-go-back` | 返回上一个 xref 位置 |
| `SPC m g d` | `fumos-find-definition` | 查找运行时 definition |
| `SPC m g D` | `fumos-find-definition-other-window` | 在其他窗口查找 definition |
| `SPC m g n` | `fumos-next-error` | 访问下一个归属当前连接的 FUMOS 错误 |
| `SPC m g N` | `fumos-previous-error` | 访问上一个归属当前连接的 FUMOS 错误 |
| `SPC m h a` | `fumos-apropos` | 搜索运行时符号 |
| `SPC m h h` | `fumos-show-documentation` | 在游戏 REPL 显示符号文档 |
| `SPC m h A` | `fumos-show-arglist` | 显示符号参数列表 |
| `SPC m h m` | `fumos-macroexpand` | 展开 point 处的 form |
| `SPC m h l` | `fumos-show-generated-lua` | 显示最近生成的 Lua |
| `SPC m r a` | `fumos-attach` | 显式选择并附着实例 |
| `SPC m r c` | `fumos-clear-repl` | 清空当前游戏 REPL |
| `SPC m r i` | `fumos-interrupt` | 中断当前求值 |
| `SPC m r q` | `fumos-disconnect` | 断开连接但不停止 Kristal |
| `SPC m r r` | `fumos-reconnect` | 重新连接同一 PID |
| `SPC m r s` | `fumos-switch-to-repl` | 显示游戏 REPL |
| `SPC m r R` | `fumos-reload-game-preserve` | 保留临时状态并重载 |
| `SPC m r L` | `fumos-reload-game-save` | 从最近存档重载 |
| `SPC m r 0` | `fumos-reload-game-from-start` | 从游戏开头重载 |

这 36 个键位只安装到 `fumos-mode` 源码 buffer。普通 Fennel 项目保留原有
localleader。额外的 REPL context 键位 `SPC m r i`（或 `M-SPC m r i`）只安装到
FUMOS 自有的 `fumos-repl-mode-map`；普通 `fennel-proto-repl-mode` buffer 会保留
FUMOS 加载前同键的精确 binding identity。

### 游戏重载模式

| 状态来源 | Wire mode | 命令 | Doom 键位 |
| --- | --- | --- | --- |
| 保留临时状态 | `temp` | `fumos-reload-game-preserve` | `SPC m R`（另有 `SPC m r R`） |
| 最近存档 | `save` | `fumos-reload-game-save` | `SPC m S`（另有 `SPC m r L`） |
| 从头开始 | `none` | `fumos-reload-game-from-start` | `SPC m 0`（另有 `SPC m r 0`） |

这些编辑器命令始终通过 FUMOS 请求同一进程内的 `Kristal.quickReload` 模式。只有
Mod 设置 `hardReset=false` 时，它们才与 Kristal 原生 `Ctrl+R`、
`Ctrl+Shift+R` 和 `Ctrl+Alt+R` 的行为完全一致。设置 `hardReset=true` 时，游戏内
原生 `Ctrl+R` 路径会调用 `love.event.quit("restart")`，重启整个 LÖVE/Kristal
引擎；编辑器键位不会模拟这种硬重启，仍会请求对应的同进程快速重载。

每个命令都会要求 FUMOS 调度 Kristal 重载，等待属于同一 PID 的 replacement
descriptor，然后附着新的 REPL session。同步或异步错误、`C-g`/`quit` 以及其他
nonlocal exit 都会取消本次操作的 descriptor polling。只有预期的
`connection-lost` transition 会让同一个 operation generation 继续等待 replacement。
默认等待 30 秒，使资源较大的 mod 有时间完成加载；项目需要其他正数 deadline 时，
可以自定义 `fumos-game-reload-timeout`。

### 连接与 buffer 隔离

FUMOS 始终使用保留 bootstrap module `fumos.repl.fennel`。对应的 upstream
module-name 变量在每个游戏 REPL 中都是 buffer-local，不会修改用户全局 Fennel
REPL 的默认值。

同时存在多个 accepted request 时，mode line 会保持 busy。
`fumos-cancel-active-request` 会在多于一个请求时显式询问 request ID，只有最后一个
`done` 到达后连接才回到 ready。

断线会取消尚未执行的普通 callback。每个尚未实际收到 values/error 终态的 request
至多收到一次 `connection-lost` 终态。看到 `done` 不等于终态已经交付；同 PID
replacement 建立后，旧连接的 timer 不能再更新新连接 UI。

FUMOS 不会接管已经占用确定性 REPL 名称的用户 buffer。发生冲突时连接会被拒绝，
该 buffer 的内容、major mode、名称和存活状态都保持不变。源码 link 使用
buffer-local owner，并保存首次从普通 REPL 切到 FUMOS 前的精确 target 与 proto-mode
状态。A 到 B 的 relink 沿用此 snapshot；clear、transport teardown 和 buffer kill
会恢复同一个仍存活的普通 target 及其原 mode。用户显式关闭 proto mode 时会恢复
target，同时保留新的 disabled 意图；后续普通 relink 的新 target 具有更高优先级。
所有清理路径都会立即释放旧 FUMOS link 和 xref ownership，并保留无关 xref
backend。

## 教学分支与兼容目标

下游 `fumos-test` 仓库提供两个教学分支：

- `main` 是可游玩的 Lua/Fennel 混合项目，包含 Fennel 弹幕、对话和与现有 Lua
  集成的示例。
- `pure-fennel` 是对应的模板分支，不包含 Mod 作者编写的 Lua。

它们是下游发布产物，不是本仓库的测试 fixture。精确 Kristal 兼容目标为：

- 0.10 基线 `752bc0688ba97ca8a256ba9125b7e05a1ca6edbd`。
- 0.11 开发基线 `8ee0129cbd18daa5334eb458096da9e0ee484ad5`。

这些 SHA 是发布目标。真实游戏/Emacs 端到端验证属于 `fumos-test` 的发布流程，
不能根据本仓库编辑器测试套件的结果推断其已经通过。

## 验证

在 `.emacs/` 或本仓库根目录运行五个组件测试入口：

```sh
make test
make test-upstream
make test-doom
make test-installer
make test-launcher
make testall
```

`make test` 是 vanilla Emacs suite；`make test-upstream` 验证 vendored upstream
快照；`make test-doom` 使用隔离的真实 Doom PTY profile；`make test-installer`
测试固定版本安装器；`make test-launcher` 检查 Kristal 启动脚本契约；`make
testall` 运行前述五项。测试数量刻意不固定；应从当次输出读取实际 ERT 数量，并要求
`0 unexpected`。

对于旧有 LuaLS 配置，以下命令仍可用于检查工作区：

```sh
lua-language-server --check=. --configpath=.emacs/luarc.json \
  --check_format=pretty
```

它可能报告项目已有诊断；这里用于确认 LuaLS 能读取工作区配置和 library。

## 许可证

本仓库采用混合许可边界：

- FUMOS Elisp、Elisp 测试、项目入口和 vendored fennel-mode 快照使用
  GPL-3.0-or-later。参见 `LICENSE-GPL-3.0-or-later` 与 `THIRD_PARTY.md`。
- 既有 LuaLS data 和 launcher 继续按仓库原有 MIT/Apache 双许可提供。参见
  `LICENSE-MIT` 与 `LICENSE-APACHE`。

不能把整个仓库描述为全部双许可或全部 GPL。第三方源码保留各自的上游许可与来源。
