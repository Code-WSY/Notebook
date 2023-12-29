# Anaconda操作汇总

## 安装Anaconda

首先可以通过wget下载安装包,如下：

```bash
wget https://repo.anaconda.com/archive/Anaconda3-2023.09-0-Linux-x86_64.sh
```

然后通过bash执行安装命令进行安装：

```bash
bash Anaconda3-2023.09-0-Linux-x86_64.sh
```

## 配置Anaconda

### 查看配置信息

可以通过`conda info`来查看当前用户的配置信息，包括：

1. `active environment`：当前激活的环境名称。

2. `active env location`：当前激活的环境所在的文件系统路径。

3. `shell level`：表示当前 shell 中嵌套的 Conda 环境层数。

   在使用 Conda 时，你可以激活不同的环境。当你激活一个新的环境时，Conda 实际上是在当前的 shell 会话中添加了一层。这种“层”的概念有助于管理和理解当前会话中激活的环境。

   - **shell level 为 1**：意味着你已经激活了一个 Conda 环境，并且没有进一步嵌套或激活其他环境。
   - **shell level 大于 1**：表示你在当前会话中嵌套激活了多个环境。比如，如果你先激活一个环境，然后在该环境中再激活另一个环境，那么 shell level 就会增加。

   **shell level 为 2**，意味着当前 shell 会话中已经嵌套激活了两个 Conda 环境。这是在一个已激活的 Conda 环境内又激活了另一个环境。

4. `user config file`：用户的 Conda 配置文件路径。

5.  `populated config files` : 被加载的 Conda 配置文件。

6. `conda version` : 当前安装的 Conda 版本。

7.  `conda-build version` : 当前安装的 conda-build 工具版本。

8. `python version` : Conda 环境中的 Python 版本。

9. `virtual packages` : 系统级虚拟包的信息。

10. `base environment` : Conda 的基础环境路径。一般是安装路径，如：/opt/anaconda3  (read only)

11. `conda av data dir/conda av metadata url` :  Conda 的自动版本控制相关的目录和元数据 URL。

12. `channel URLs `: Conda 搜索和安装包的渠道 URL。

13. `package cache` : Conda 包的缓存目录。

14. `envs directories` : 存储 Conda 环境的目录。

15. `platform` : 当前操作系统平台。

16. `user-agent`: Conda 的用户代理信息，包括 Conda 版本、请求库版本、Python 版本、操作系统版本等。

17. `UID:GID` : 当前用户的用户ID和组ID。

18. `netrc file` : 网络资源文件，用于存储网络服务的登录信息。

19. `offline mode` : 显示 Conda 是否在离线模式下运行。

### `.condarc`文件

conda 配置文件`.condarc/condarc`是一个可选的运行时配置文件，允许高级用户配置 conda 的各个方面，例如在哪些通道中搜索包、代理设置和环境目录。

`.condarc`文件可以更改许多参数，包括：

- conda 寻找包的地方。
- conda 是否以及如何使用代理服务器。
- 其中 conda 列出了已知环境。
- 是否使用当前激活的环境名称更新 Bash 提示符。
- 用户构建的包是否应上传到 [Anaconda.org](http://anaconda.org/)。
- 新环境中要包含哪些默认包或功能。

### conda如何搜索配置文件

onda 在以下位置查找文件`.condarc`：

```python
if on_win:
    SEARCH_PATH = (
        "C:/ProgramData/conda/.condarc",
        "C:/ProgramData/conda/condarc",
        "C:/ProgramData/conda/condarc.d",
    )
else:
    SEARCH_PATH = (
        "/etc/conda/.condarc",
        "/etc/conda/condarc",
        "/etc/conda/condarc.d/",
        "/var/lib/conda/.condarc",
        "/var/lib/conda/condarc",
        "/var/lib/conda/condarc.d/",
    )

SEARCH_PATH += (
    "$CONDA_ROOT/.condarc",
    "$CONDA_ROOT/condarc",
    "$CONDA_ROOT/condarc.d/",
    "$XDG_CONFIG_HOME/conda/.condarc",
    "$XDG_CONFIG_HOME/conda/condarc",
    "$XDG_CONFIG_HOME/conda/condarc.d/",
    "~/.config/conda/.condarc",
    "~/.config/conda/condarc",
    "~/.config/conda/condarc.d/",
    "~/.conda/.condarc",
    "~/.conda/condarc",
    "~/.conda/condarc.d/",
    "~/.condarc",
    "$CONDA_PREFIX/.condarc",
    "$CONDA_PREFIX/condarc",
    "$CONDA_PREFIX/condarc.d/",
    "$CONDARC",
)
```

XDG_CONFIG_HOME 是根据 XDG 基础目录规范（XDGBDS）定义的用于存储用户特定配置文件的路径。应该使用默认的 $HOME/.config。CONDA_ROOT 是基础 Conda 安装的路径。CONDA_PREFIX 是当前激活环境的路径。CONDARC 必须是指向名为 .condarc、condarc 的文件的路径，或者是以 YAML 后缀（.yml 或 .yaml）结尾的文件路径。

### 优先级

构建 conda 配置的优先级如下所示。每个新箭头优先于之前的箭头。

![conda配置的优先级](https://conda.io/projects/conda/en/latest/_images/config-precedence.png)

Conda 配置文件的解析顺序如下：

1. **Config files (by parse order)**: 指的是 Conda 会按照一定的顺序来解析配置文件。这个顺序通常是先解析默认的配置文件，然后是用户级的 `.condarc` 文件，最后是系统级的 `.condarc` 文件。
2. **Config files specified by $CONDARC**: 如果设置了环境变量 `$CONDARC`，指向一个或多个配置文件，Conda 将解析这些文件中的配置。这允许用户指定一个自定义的配置文件，覆盖默认的解析顺序。
3. **Command line parameter**: 命令行参数会覆盖在配置文件中指定的配置。因为这些参数是直接在命令行中提供的，所以它们具有较高的优先级。
4. **Environment variables**: 最后，环境变量中的设置具有最高优先级。这意味着，无论在配置文件中或命令行参数中设置了什么，环境变量中的设置都会覆盖它们。

### 修改配置信息

要修改 Conda 的配置信息，你可以使用 Conda 的配置命令 (`conda config`) 或直接编辑配置文件。下面是一些常见的修改方式：

#### 使用命令行

1. **添加或移除通道 (channel)**:
   - 添加: `conda config --add channels [channel_name]`
   - 移除: `conda config --remove channels [channel_name]`
2. **设置默认通道**:
   - `conda config --set default_channels [channel_url]`
3. **禁用/启用自动激活基础环境**:
   - 禁用: `conda config --set auto_activate_base false`
   - 启用: `conda config --set auto_activate_base true`
4. **设置包缓存路径**:
   - `conda config --set pkgs_dirs [path_to_directory]`
5. **设置环境目录**:
   - `conda config --set envs_dirs [path_to_directory]`
6. **开启/关闭离线模式**:
   - 开启: `conda config --set offline true`
   - 关闭: `conda config --set offline false`

#### 编辑配置文件

你还可以直接编辑位于你的主目录下的 `.condarc` 文件来更改配置。这个文件是 YAML 格式的，你可以用任何文本编辑器打开并编辑它。例如：

```yaml
channels: # 指定用于搜索和安装包的通道顺序列表
  - defaults
  - conda-forge
  
default_channels: #指定默认通道的URL。
  - [URL]
  - [...]
  
custom_channels: # 自定义通道的URL。
  conda-forge: [URL] 
  
envs_dirs: #环境目录位置
  - /path/to/envs

pkgs_dirs: #包缓存的位置。
  - /path/to/pkgs
  
create_default_packages: #创建环境时默认的安装包
- numpy
- pandas

auto_activate_base: false # 不自动激活base环境。
channel_priority: flexible #Conda将尝试在所有配置的通道中找到最佳的包匹配，而不是严格按照通道的优先级顺序。
#channel_priority: strict #严格按照通道的优先级顺序。
show_channel_urls: true #在Conda操作输出中显示通道URL

```

这里，你可以按照 YAML 格式添加或修改配置项。

从自定义通道安装包:

```shell
conda install numpy -c conda-forge -c bioconda
```

Conda 会首先在 `conda-forge` 通道搜索 `numpy`，如果没有找到，再在 `bioconda` 通道中搜索。

#### 注意事项

- 在对配置进行更改之前，最好备份当前的 `.condarc` 文件，以便在出现任何问题时能够恢复。
- 某些更改可能需要重启 shell 会话或打开新的终端窗口才能生效。
- 确保遵循 YAML 格式规则，特别是在直接编辑 `.condarc` 文件时。错误的格式可能导致 Conda 无法正确读取配置。

Anaconda在安装完成时会询问是否将安装路径添加到系统的环境变量中，这是负责在打开新的 shell 会话时自动**初始化 Conda 环境**，确保 Conda 和其管理的环境和工具可以被正确地使用。

```bash
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/opt/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"  
if [ $? -eq 0 ]; then
    eval "$__conda_setup"  #执行$__conda_setup中的命令
else
    if [ -f "/opt/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/opt/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/opt/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
```

