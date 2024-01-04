# Linux基本命令指南

## 查看系统信息

- `lsb_release -a`：显示版本信息。
- `uname -r`：显示内核版本。
- `hostnamectl`：显示系统信息，包括主机名、操作系统、内核等。

## 网络配置

- `ifconfig` 或 `ip addr`：显示所有网络接口的IP地址。
- `ping [地址]`：测试与指定地址的网络连接。
- `netstat -tuln`：显示所有监听端口。

## 执行脚本

#### 1. 直接执行 (`./`)

使用 `./script.sh` 来直接执行脚本。这要求脚本具有执行权限（通过 `chmod +x script.sh` 设置）。

**示例**：

```bash
./script.sh
```

**特点**：

- 启动新的子shell执行脚本。
- 脚本中设置的环境变量不会影响当前shell。

#### 2. 使用 `bash` 或 `sh`

使用 `bash script.sh` 或 `sh script.sh` 执行脚本。这不需要脚本有执行权限。

**示例**：

```bash
bash script.sh
```

**特点**：

- 以指定的shell（bash或sh）启动新的子shell执行脚本。
- 脚本中设置的环境变量不会影响当前shell。

#### 3. 使用 `source` 或 `.`

使用 `source script.sh` 或 `. script.sh` 执行脚本。

**示例**：

```bash
source script.sh
```

**特点**：

- 在当前shell环境中执行脚本，不启动新的子shell。
- 脚本中设置的环境变量会影响当前shell，如代理设置。

### 比较

- `./` 和 `bash`/`sh`：都会启动新的子shell来运行脚本，脚本对环境变量的修改不会影响原始shell。
- `source` 或 `.`：在当前shell中执行脚本，对环境变量的任何修改都会影响当前shell，适用于需要持续修改当前shell环境的场景。

## 文件下载

- `wget [URL]`：从指定URL下载文件。
- `curl -O [URL]`：使用curl下载文件。
- 

| 特性             | wget                                                         | curl                                                         |
| ---------------- | ------------------------------------------------------------ | ------------------------------------------------------------ |
| **基本功能**     | 主要用于下载文件，支持递归下载，适用于HTTP、HTTPS和FTP协议。 | 支持更多协议（如FTP, FTPS, HTTP, HTTPS等），用于上传和下载数据，可以测试HTTP请求。 |
| **数据传输**     | 默认保存下载文件到当前目录，支持断点续传。                   | 默认输出到标准输出，除非指定输出文件。                       |
| **使用场景**     | 适合简单下载操作和批量下载（如整个网站）。                   | 适合复杂网络交互，如处理HTTP请求、表单数据、认证等。         |
| **递归下载**     | 支持递归下载。                                               | 不支持递归下载。                                             |
| **脚本/API交互** | 一般不用于脚本编写和API交互。                                | 常用于脚本编写和API交互，能构造各种HTTP请求。                |
| **数据传输控制** | 在下载大文件时易于使用，自动处理重试和断点续传。             | 在上传操作和自定义HTTP请求处理方面更强大。                   |

## 文件和目录操作

- `ls`：列出目录内容。
- `cd [目录]`：切换到指定目录。
- `mkdir [目录]`：创建一个新目录。
- `rm [文件]`：删除文件或目录。

## 安装和管理软件包

- `sudo apt update`：更新软件包列表。
- `sudo apt upgrade`：升级所有可升级的软件包。
- `sudo apt install [包名]`：安装软件包。
- `sudo apt remove [包名]`：卸载软件包。

### 1. 软件包管理系统

Linux 分发版通常使用以下软件包管理系统之一：

- **Debian/Ubuntu**: 使用 `apt` 或 `dpkg`。
- **Fedora/RHEL/CentOS**: 使用 `yum` 或 `dnf`。
- **Arch Linux**: 使用 `pacman`。

### 2. 安装软件包

使用包管理工具可以从仓库安装软件包。

- **Debian/Ubuntu**（使用 `apt`）:
  - 更新软件包列表：`sudo apt update`
  - 安装软件包：`sudo apt install 包名`
- **Fedora/RHEL/CentOS**（使用 `dnf` 或 `yum`）:
  - 更新软件包列表：`sudo dnf check-update`（`yum` 使用 `sudo yum check-update`）
  - 安装软件包：`sudo dnf install 包名`（`yum` 使用 `sudo yum install 包名`）
- **Arch Linux**（使用 `pacman`）:
  - 更新软件包列表并升级所有包：`sudo pacman -Syu`
  - 安装软件包：`sudo pacman -S 包名`

### 3. 管理软件包

包括升级、卸载和清理软件包。

- **升级软件包**:
  - Debian/Ubuntu：`sudo apt upgrade`
  - Fedora/RHEL/CentOS：`sudo dnf upgrade`（`yum` 使用 `sudo yum update`）
  - Arch Linux：`sudo pacman -Syu`
- **卸载软件包**:
  - Debian/Ubuntu：`sudo apt remove 包名`
  - Fedora/RHEL/CentOS：`sudo dnf remove 包名`（`yum` 使用 `sudo yum remove 包名`）
  - Arch Linux：`sudo pacman -R 包名`
- **清理缓存**:
  - Debian/Ubuntu：`sudo apt clean`
  - Fedora/RHEL/CentOS：`sudo dnf clean all`（`yum` 使用 `sudo yum clean all`）
  - Arch Linux：`sudo pacman -Sc`

### 4. 镜像源的设置与管理

修改软件源可以加速软件包的下载速度，特别是选择地理位置较近的镜像源。

- **修改镜像源**:

  - Debian/Ubuntu：编辑 `/etc/apt/sources.list` 文件。
  - Fedora/RHEL/CentOS：编辑 `/etc/yum.repos.d/` 或 `/etc/dnf/dnf.conf` 中的仓库文件。
  - Arch Linux：编辑 `/etc/pacman.d/mirrorlist` 文件。

- **示例**：更换 Ubuntu 的镜像源

  ```bash
  sudo vim /etc/apt/sources.list
  # 添加或修改为新的镜像源地址
  
  ```

  格式大致如下：

  ```bash
  # 默认的 Ubuntu 仓库
  deb http://us.archive.ubuntu.com/ubuntu/ focal main restricted
  deb http://us.archive.ubuntu.com/ubuntu/ focal-updates main restricted
  deb http://us.archive.ubuntu.com/ubuntu/ focal universe
  deb http://us.archive.ubuntu.com/ubuntu/ focal-updates universe
  deb http://us.archive.ubuntu.com/ubuntu/ focal multiverse
  deb http://us.archive.ubuntu.com/ubuntu/ focal-updates multiverse
  deb http://us.archive.ubuntu.com/ubuntu/ focal-backports main restricted universe multiverse
  
  # 安全更新
  deb http://security.ubuntu.com/ubuntu focal-security main restricted
  deb http://security.ubuntu.com/ubuntu focal-security universe
  deb http://security.ubuntu.com/ubuntu focal-security multiverse
  
  # 可选：添加第三方软件仓库
  # deb http://example.com/ubuntu focal main
  ```

  **修改格式：**

  将`us.archive.ubuntu.com` 和 `security.ubuntu.com` 等地址替换为您选择的镜像源地址。

  例如，使用清华大学的镜像源，您可以将`us.archive.ubuntu.com` 替换`mirrors.tuna.tsinghua.edu.cn`。

  更新软件包列表：

  ```shell
  sudo apt update
  ```

### 5. 注意事项

- 在修改镜像源前，最好备份原始的 `sources.list` 文件。
- 选择靠近您地理位置的镜像源，可以加快下载速度。
- 确保所选的镜像源支持您的 Ubuntu 版本。
- 修改镜像源后，执行 `sudo apt update` 来更新软件包列表是必须的。

## 系统管理

- `top` 或 `htop`：显示当前运行的进程及其资源占用情况。
- `df -h`：显示磁盘使用情况。
- `free -m`：显示内存使用情况。
- `lsblk`：用于列出所有可用的块设备，包括它们的大小、类型、挂载点等信息。这个命令显示的是设备级别的信息，可以让您看到整个系统的磁盘和分区情况。
- `fdisk -l`：（列出分区表）提供了关于磁盘分区的详细信息，包括每个分区的总容量。这个命令需要管理员权限。

## 进程管理

### 1. 查看进程

使用以下命令可以查看在 Linux 系统上运行的进程：

- `ps aux`: 显示所有运行中的进程。
- `top` / `htop`: 提供实时的进程监控视图。

#### 进程参数

在 Linux 系统中使用 `ps aux` 或类似命令查看进程时，输出的每列参数具有特定的含义：

1. **USER**: 进程所属的用户名称。这显示了哪个用户启动了该进程。
2. **PID**: Process ID（进程标识符）。这是一个唯一的数字，用于标识系统中的每个进程。
3. **%CPU**: 进程使用的 CPU 百分比。表示该进程占用的 CPU 时间与总可用 CPU 时间的比例。
4. **%MEM**: 进程使用的内存百分比。表示该进程占用的物理内存与总物理内存的比例。
5. **VSZ**: Virtual Memory Size（虚拟内存大小），单位是 KB。表示该进程已分配的虚拟内存总量，包括所有的代码、数据和共享库加上交换空间。
6. **RSS**: Resident Set Size（常驻集大小），单位是 KB。这是该进程当前占用的物理内存大小，不包括交换出去的内存部分。
7. **TTY**: 终端类型。这列显示了与进程关联的终端（如 pts/0）。如果显示为 '?'，则表示进程没有关联的终端。
8. **STAT**: 进程状态。这个字段显示了进程的状态，如运行（R）、休眠（S）、停止（T）等。
9. **START**: 进程启动时的时间。这表示进程开始运行的时间。
10. **TIME**: 进程使用的累计 CPU 时间，通常以分:秒的格式显示。
11. **COMMAND**: 启动进程的命令名或命令行。显示了用于启动进程的命令及其参数。

### 2. 进程状态

`ps` 命令会展示不同的进程状态，每个状态代表不同的含义：

| 状态 | 描述                                          |
| ---- | --------------------------------------------- |
| R    | Running - 进程正在运行或等待运行              |
| S    | Sleeping - 进程处于休眠状态                   |
| T    | Stopped - 进程已被停止                        |
| I    | Idle - 内核级线程的闲置状态                   |
| Z    | Zombie - 僵尸进程，已结束但未被父进程回收     |
| D    | Uninterruptible Sleep - 不可中断的休眠状态    |
| Ss   | Session Leader - 会话领导者且处于休眠状态     |
| <    | High-priority - 高优先级进程                  |
| N    | Low-priority - 低优先级进程                   |
| L    | Pages Locked - 进程的部分或全部页被锁定在内存 |
| s    | Session Leader - 会话的领导者                 |
| l    | Multi-threaded - 多线程进程                   |
| +    | Foreground - 属于前台进程组                   |

### 3. 管理进程

进程管理主要涉及结束或控制进程的执行：

- **结束进程**：
  - `kill [进程ID]`: 发送 SIGTERM 信号以优雅地结束进程。
  - `killall [进程名称]`: 结束所有名为指定名称的进程。
  - `pkill [模式]`: 根据模式匹配结束进程。
- **控制进程**：
  - `nice` / `renice`: 调整进程的优先级。
  - `bg`: 将一个停止的进程放到后台继续运行。
  - `fg`: 将一个后台进程放到前台继续运行。
- **查找进程**：
  - `pgrep [模式]`: 根据模式匹配查找进程。

### 4. 后台运行

在 Linux 系统中，后台运行进程是一种常用的操作，它允许您在不占用当前终端的情况下执行程序。理解这一点对于有效管理长时间运行的进程或远程会话非常重要。

1. **使用 `&` 符号**：

   - **用法**：在命令末尾加上 `&`。
   - **特点**：简单快捷，适用于临时后台执行。
   - **限制**：终端关闭后，进程可能被终止。

2. **使用 `nohup` 命令**：

   - **用法**：`nohup your_command > output.log 2>&1 &`。
   - **特点**：可以防止终端关闭时进程被终止。
   - **输出重定向**：可将输出重定向到指定文件，如 `output.log`。

3. **使用 `screen` 或 `tmux`**：

   - **用法**：`screen` 或 `tmux` 新建会话，然后在会话中运行命令。
   - **特点**：提供了会话管理，适合长时间运行和复杂任务。
   - **管理**：允许用户断开连接后再重新连接到会话。

   **比较表格**：

   | 方法            | 用法                           | 特点                 | 适用场景                 |
   | --------------- | ------------------------------ | -------------------- | ------------------------ |
   | `&`             | `command &`                    | 简单快速             | 短暂后台运行             |
   | `nohup`         | `nohup command > output.log &` | 防止终端关闭影响进程 | 长时间运行，需要日志记录 |
   | `screen`/`tmux` | `screen` 后运行命令            | 会话管理，可断开重连 | 复杂任务，远程操作       |

### 5. 进程嵌套

#### 基础概念

**进程**：运行中的程序实例。每个进程都有一个唯一的进程标识符（PID）。

**父子进程**：创建新进程时，原始进程成为父进程，新进程成为子进程。

**Shell 进程**：用户与操作系统交互的界面。在 Shell 中执行命令会创建新的子进程。

#### 进程嵌套

1. **嵌套概述**：当一个进程（通常是 Shell）启动另一个进程时，发生进程嵌套。子进程继承父进程的一些属性，但在执行期间独立于父进程。
2. **示例**：
   - 在 Shell 中执行 `bash` 会启动一个新的子 Shell。
   - 运行 `ssh user@host` 会创建一个 SSH 进程，该进程打开远程主机上的另一个 Shell。

#### 进程与会话

1. **会话管理**：Shell 会话开始于用户登录，结束于用户登出或执行 `exit` 命令。每个会话可以包含多个进程。
2. **进程继承**：子进程继承父进程的环境变量和上下文，但在父进程执行 `exit` 后通常会被终止。
3. **后台进程**：使用 `&` 符号启动的进程会在后台运行，例如 `some_command &`。这些进程与当前 Shell 解耦，允许 Shell 启动其他任务。

#### 进程通信

1. **管道和重定向**：Shell 中的管道 `|` 和重定向 `>` 用于进程间通信，例如 `command1 | command2`。
2. **信号**：如 `kill` 命令发送信号到其他进程，用于控制进程行为，例如终止进程。

#### 终端会话

1. **会话控制**：Shell 会话允许用户与系统交互。关闭终端窗口或执行 `exit` 会结束当前 Shell 会话。
2. **子 Shell**：在 Shell 会话中启动的 Shell（如通过 `bash` 命令）是父 Shell 的子进程。执行 `exit` 会返回到父 Shell。

通过理解进程嵌套和会话管理，用户可以更有效地控制和管理系统中的进程和任务。

### 6. 注意事项

- 在结束重要进程前，确认进程的功能和重要性。
- 对于系统进程和关键服务，谨慎操作以防止系统不稳定。

## 储存管理

- `lsblk`：列出所有可用的块设备，如硬盘、USB设备等，显示设备的大小、类型、挂载点等信息。这是识别和管理存储设备的重要工具。
- `fdisk -l`：显示磁盘分区表。
  - 示例：`sudo fdisk -l`
  - 功能：显示所有磁盘的分区表。这个命令需要管理员权限，可以显示每个磁盘的分区大小、类型等详细信息。
- `mount`/`umount`：分别用于挂载和卸载文件系统。`mount` 命令将存储设备连接到文件系统的特定点，而 `umount` 则用于安全地断开连接。
  - 挂载示例：`sudo mount /dev/sdb1 /mnt/mydisk`
  - 卸载示例：`sudo umount /mnt/mydisk`
  - 功能：`mount`命令用于将存储设备挂载到文件系统的特定目录，而`umount`用于从文件系统中卸载它。
- `df -h`：显示文件系统的磁盘空间使用情况。这个命令以易读的格式显示每个挂载的文件系统所使用的和可用的磁盘空间。
  - 示例：`df -h`
  - 功能：以人类可读的格式显示文件系统的磁盘使用情况。它会列出每个挂载的文件系统的总空间、已用空间和可用空间。
- `du`：显示指定目录的磁盘使用量。这个命令可以帮助用户了解特定文件或目录占用的存储空间。
  - 示例：`du -sh /path/to/directory`
  - 功能：显示指定目录的磁盘使用量。在这个例子中，`-s`表示汇总显示，`-h`表示以易于阅读的格式显示（如KB、MB、GB）。
- `mkfs`：用于格式化磁盘分区。这个命令可以用来创建一个新的文件系统，例如`mkfs.ext4`用于创建ext4类型的文件系统。
  - 示例：`sudo mkfs.ext4 /dev/sdc1`
  - 功能：格式化指定的分区或磁盘。在这个例子中，我们使用`mkfs.ext4`来在`/dev/sdc1`上创建一个新的ext4文件系统。

## 用户管理

- `sudo -i`：登录root账户
- `sudo su - username`  切换到指定用户，需要输入该用户的密码，如果不带参数，它将切换到root用户。
- `sudo adduser  username`：添加新用户。这个命令不仅创建用户，还可以创建用户的主目录、复制配置文件等。
- `passwd`：修改当前用户的密码
  - `sudo passwd 用户名`：修改用户的密码。如果是管理员修改其他用户的密码，需要指定用户名。
- `usermod`：修改用户的基本信息，如用户名、主目录等。
  - `sudo usermod -l 新用户名 旧用户名`：使用`usermod`命令来修改用户的基本信息，如用户名、主目录等。
  - `sudo usermod -d 新主目录 -m 用户名`：
- `deluser`：删除用户
  - `sudo deluser --remove-home 用户名`：删除用户的主目录和邮件目录
  - `sudo userdel [用户名]`：删除用户账户，但不会删除用户的主目录。

## 权限管理

### 1. Linux 默认的权限配置

Linux 系统中的权限主要分为三类：用户（u）、组（g）和其他（o）。每个文件或目录都有与之关联的权限，决定了用户对这些文件或目录的访问能力。

- **权限类型**：
  - **读（r）**：允许读取文件内容或列出目录内容。
  - **写（w）**：允许修改文件内容或目录中的文件。
  - **执行（x）**：允许执行文件或搜索目录。
- **文件和目录**：
  - 文件的权限直接影响其可读写和执行性。
  - 目录的权限影响能否列出、访问、添加、删除其内部的文件。

### 2. 查看权限

使用 `ls -l` 命令可以查看文件或目录的权限。

- 示例

  ```shell
  ls -l filename
  ```

### 3. 修改权限

权限可以通过 `chmod` 命令来修改。

- **使用符号方式修改权限**：
  - 格式：`chmod [u/g/o/a][+-=][r/w/x] 文件名`
  - `u` 代表用户，`g` 代表组，`o` 代表其他，`a` 代表所有。
  - `+` 表示添加权限，`-` 表示移除权限，`=` 表示设置精确权限。
  - **示例**：给用户添加执行权限 `chmod u+x filename`
- **使用数值方式修改权限**：
  - 权限的数值表示：读（4）、写（2）、执行（1）。
  - 各类用户的权限通过相加得到一个三位数。
  - **示例**：设置文件所有者可读写执行（7），组可读执行（5），其他可执行（1）的权限 `chmod 751 filename`

### 4. 修改所有权

- 使用 `chown` 修改文件或目录的所有者。
- 使用 `chgrp` 修改文件或目录的所属组。
- **示例**：
  - 改变文件所有者：`chown newowner filename`
  - 改变文件所属组：`chgrp newgroup filename`

### 5. 特殊权限

- **SUID**：设置用户ID。当执行该文件时，进程将具有文件所有者的权限。
- **SGID**：设置组ID。对于文件，执行时将具有文件组的权限；对于目录，新建的文件将继承该目录的组。
- **Sticky Bit**：通常用于目录，防止用户删除不属于他们的文件。
- **示例**：设置 SUID `chmod u+s filename`

### 6. 注意事项

- 在修改重要文件或目录权限时应特别小心，错误的权限设置可能导致安全问题或功能异常。
- 对于系统关键目录（如 `/etc`、`/bin` 等），最好不要更改其默认权限。

## 配置文件

在Linux系统中，开机或用户登录时会执行的配置文件。

### 用户默认配置文件

在 Linux 系统中，当创建一个新用户时，默认的 `.bashrc，.profile` 等配置文件是从 `/etc/skel` 目录复制而来的。`/etc/skel` 目录通常包含了一些基本的配置文件，这些文件将被复制到新用户的家目录中。

因此，要在每个新建用户的 `.bashrc` 中默认写入一些内容，你可以修改 `/etc/skel/.bashrc` 文件。

### 登录shell与非登录shell

1. **登录shell**：当你**物理登录**到系统上（例如在登录界面输入用户名和密码）或通过远程登录（如SSH）时，你首次得到的shell是登录shell。

2. **非登录shell**：当你在图形界面中打开一个新的终端窗口或在已存在的shell中启动一个新的shell（例如输入`bash`命令）时，你得到的是非登录shell。

   （在一个shell内部可以通过`bash`命令启动嵌套第二个shell。在这个第二个shell里，你可以执行命令，就像在第一个shell中一样并且**不会影响第一个shell会话**，可以通过输入`exit`退出并带回到第一个shell中。）

### 系统级别

1. **`/etc/profile`**

   - 这是系统级别的全局配置文件，影响所有用户。
   - 它在登录时执行，为所有用户设置环境变量。

   这是一个为所有Bash shell用户执行的脚本文件。它在登录时执行，适用于设置全局环境变量和执行全局shell脚本。如果要设置的环境变量需要运行某些命令来获取（例如使用`$(command)`），`/etc/profile`是合适的选择。

2. **`/etc/bash.bashrc`**（仅限于Bash Shell）

   - 针对Bash shell的全局配置。
   - 在每个Bash shell会话开始时执行。

   这是针对所有Bash交互式非登录shell的全局配置文件。它在每次新打开的非登录Bash shell中执行，适用于设置shell选项和别名。对于非登录shell（如打开新的终端窗口），这个文件会被读取。


### 用户级别（用户登录时执行）

1. **`~/.bash_profile`** 或 **`~/.profile`** 或 **`~/.bash_login`**
   - 这些是用户级别的配置文件，仅影响当前用户。
   - 如果存在`~/.bash_profile`，它将被首先执行。
   - 如果`~/.bash_profile`不存在，`~/.profile`或`~/.bash_login`（如果存在）将被执行。
   - 这些文件在用户登录时执行，用于设置个人的环境变量和启动程序。
2. **`~/.bashrc`**
   - 用户级别的Bash shell配置文件。
   - 在每个新开启的Bash shell中执行，通常用于设置shell选项、别名和函数。

### 优先级

- **系统级别 > 用户级别**: 首先执行系统级别的配置（`/etc/profile`，`/etc/bash.bashrc`），然后执行用户级别的配置（`~/.bash_profile`，`~/.bashrc`等）。
- 登录shell vs 非登录shell:
  - 登录shell（例如，用户刚登录时的shell）首先执行`/etc/profile`，然后是`~/.bash_profile`，`~/.profile`或`~/.bash_login`。
  - 非登录shell（例如，打开一个新的终端窗口）通常只执行`~/.bashrc`。

### 注意点

- **不同的shell可能有不同的配置文件**：以上讨论主要基于Bash shell。其他shell（如Zsh或Fish）可能有不同的配置文件。
- **某些文件可能会调用其他文件**：例如，`~/.bash_profile`可能会包含一个命令来执行`~/.bashrc`。
- **定制和覆盖**：用户可以在自己的配置文件中覆盖或扩展系统级别的设置。

理解这些配置文件及其优先级有助于更好地管理Linux环境中的用户和系统行为。

- 全局配置文件：存在于`/etc/profile`，`/etc/bash.bashrc`
- 查看环境：`echo $PATH`

## 文件目录

<img src="http://sy0316.oss-cn-hangzhou.aliyuncs.com/img/image-20231227162728608.png" alt="Linux系统文件目录" style="zoom:67%;" />

| 文件夹                     | 描述                                                         |
| -------------------------- | ------------------------------------------------------------ |
| `/bin`                     | 包含用户的基本二进制程序（如 ls, cat 等）。对所有用户可用。  |
| `/sbin`                    | 存放系统管理和维护的必需二进制程序，如启动和修复工具。只有 root 或需要特定权限的用户可用。 |
| `/etc`                     | 包含系统配置文件。这些文件由系统管理员编辑，控制系统的行为。 |
| `/lib`、`/lib32`、`/lib64` | 存放系统库文件和内核模块。`/lib32` 和 `/lib64` 分别用于32位和64位库。 |
| `/usr`                     | 包含用户程序和数据。类似于 Windows 下的 Program Files，包括 `/usr/bin`、`/usr/sbin`、`/usr/local` 等子目录。 |
| `/home`                    | 用户的个人文件夹。每个用户都有一个对应的目录。               |
| `/root`                    | root 用户的家目录。                                          |
| `/var`                     | 存放经常变化的文件，如日志、数据库等。                       |
| `/tmp`                     | 用于存放临时文件。系统重启时，这些文件可能会被删除。         |
| `/boot`                    | 包含启动 Linux 系统所需的文件，如内核、引导加载程序等。      |
| `/dev`                     | 包含设备文件，这些文件代表系统中的硬件设备。                 |
| `/proc`                    | 虚拟文件系统，提供对内核和进程信息的访问。                   |
| `/sys`                     | 另一个虚拟文件系统，用于与内核交互。                         |
| `/media`                   | 用于挂载可移除媒体，如 CD-ROMs、USB 驱动器等。               |
| `/mnt`                     | 通常用于临时挂载文件系统。                                   |
| `/opt`                     | 用于存放可选的应用软件包和数据文件。                         |
| `/run`                     | 用于存储系统运行时的数据，如套接字和进程 ID，通常在启动时创建。 |
| `/srv`                     | 存放服务相关的数据，如 FTP 或 Web 服务器的数据。             |
| `lost+found`               | 当系统意外崩溃或机器非正常关机时，文件系统检查(fsck)的恢复文件存放地。 |
| `/snap`                    | 用于存放 Snappy 软件包管理器的应用程序和数据。               |

## 帮助和文档

- `man [命令]`：显示命令的手册页。
- `info [命令]`：显示命令的info文档。

