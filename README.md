# Linux(Ubuntu)基本命令指南（待更新）

Ubuntu是一个基于Debian的Linux操作系统，广泛用于服务器和桌面系统。本指南旨在介绍一些常用的Ubuntu命令。

## 查看系统信息

- `lsb_release -a`：显示Ubuntu版本信息。
- `uname -r`：显示内核版本。
- `hostnamectl`：显示系统信息，包括主机名、操作系统、内核等。

## 网络配置

- `ifconfig` 或 `ip addr`：显示所有网络接口的IP地址。
- `ping [地址]`：测试与指定地址的网络连接。
- `netstat -tuln`：显示所有监听端口。

## 文件下载

- `wget [URL]`：从指定URL下载文件。
- `curl -O [URL]`：使用curl下载文件。

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

## 系统管理

- `top` 或 `htop`：显示当前运行的进程及其资源占用情况。
- `df -h`：显示磁盘使用情况。
- `free -m`：显示内存使用情况。
- `lsblk`：用于列出所有可用的块设备，包括它们的大小、类型、挂载点等信息。这个命令显示的是设备级别的信息，可以让您看到整个系统的磁盘和分区情况。
- `fdisk -l`：（列出分区表）提供了关于磁盘分区的详细信息，包括每个分区的总容量。这个命令需要管理员权限。

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

![Linux系统文件目录](http://sy0316.oss-cn-hangzhou.aliyuncs.com/img/image-20231227162728608.png)

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

