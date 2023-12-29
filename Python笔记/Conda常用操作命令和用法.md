# conda常用命令和操作

## 1. 安装和更新

- `conda install package_name`: 安装指定的软件包。
- `conda update package_name`: 更新已安装的软件包。
- `conda update conda`：将conda本身更新到最新可用版本。
- `conda upgrade conda`: 会卸载旧版本的conda，并安装新版本的conda。在遇到conda自身的问题，或者需要切换到一个全新的conda版本时，才推荐使用`conda upgrade conda`。
- `conda env remove --name myenv`：删除指定环境

## 2. 创建和管理环境

- `conda create --name env_name package_name`: 创建一个新的conda环境，并安装指定的软件包。
- 
- `conda activate env_name`: 激活指定的conda环境。
- `conda deactivate`: 退出当前的conda环境。
- `conda env list`: 列出所有已创建的conda环境。
- `conda env export > environment.yml`: 将当前环境的配置导出为一个YAML文件。
- `conda env create -f environment.yml`: 根据YAML文件创建一个新的conda环境。

## 3. 管理软件包

- `conda list`: 列出当前环境中已安装的所有软件包。
- `conda search package_name`: 在conda仓库中搜索指定的软件包。
- `conda install package_name=version`: 安装指定版本的软件包。
- `conda remove package_name`: 卸载指定的软件包。
- `conda clean --all`: 清理conda环境，删除缓存、未使用的包、旧版本的包和临时文件。
- `conda install --dry-run package_name`: 模拟安装指定的软件包，并显示可能的依赖冲突。
- `conda install --update-all package_name`: 更新所有与指定软件包存在依赖关系的软件包。

## 4. 管理频道和源

- `conda config --show channels`: 显示当前配置的所有频道。

- `conda config --add channels channel_name`: 添加一个新的频道。

- `conda config --remove channels channel_name`: 移除指定的频道。

- `conda config --set default_channels channel_name`: 设置默认的频道。

- `conda config --show`: 显示当前的配置信息。

- `conda config --get`: 获取特定配置项的值。

- `conda config --set`: 设置特定配置项的值。

  

## 5. 其他常用命令

- `conda info`: 显示conda的详细信息。
- `conda clean --help`: 查看`conda clean`命令的帮助信息。
  - `conda clean --all`: 清理conda环境，删除缓存、未使用的包、旧版本的包和临时文件。
  - `conda clean --index-cache`: 清理索引缓存，删除已过期的索引文件。
  - `conda clean --lock`: 清理锁定文件，解锁由conda进程锁定的文件。
  - `conda clean --packages`: 清理未使用的包，删除未被任何环境使用的软件包。
  - `conda clean --tarballs`: 清理tarballs，删除已下载的软件包压缩文件。
- `conda --version`: 显示conda的版本号。

## pip镜像

### 南大pip镜像

```python
pip config set global.index-url https://mirror.nju.edu.cn/pypi/web/simple/
```

### 清华pip镜像

```python
pip config set global.index-url https://pypi.tuna.tsinghua.edu.cn/simple
```

### 阿里pip镜像

```python
pip config set global.index-url https://mirrors.aliyun.com/pypi/simple/
```

### 腾讯pip镜像

```python
pip config set global.index-url http://mirrors.cloud.tencent.com/pypi/simple/
```

### 豆瓣pip镜像

```python
pip config set global.index-url http://pypi.doubanio.com/simple
```

### 网易pip镜像

```python
pip config set global.index-url https://mirrors.163.com/pypi/simple/
```

### 临时使用

另外如果临时使用可以pip的时候在后面加上-i参数，指定pip源：

```python
pip install package -i https://mirrors.aliyun.com/pypi/simple/
```

## conda镜像

### 南大镜像

```python
conda config --add channels https://mirrors.nju.edu.cn/anaconda/pkgs/free/
conda config --add channels https://mirrors.nju.edu.cn/anaconda/pkgs/main/
conda config --add channels https://mirrors.nju.edu.cn/anaconda/cloud/conda-forge/
conda config --set show_channel_urls yes
```

Conda 在执行诸如 conda install 或 conda update 等命令时，会显示软件包通道的 URL 地址。

### 清华conda镜像

```python
conda config --add channels https://mirrors.tuna.tsinghua.edu.cn/anaconda/pkgs/free/
conda config --add channels https://mirrors.tuna.tsinghua.edu.cn/anaconda/pkgs/main/
conda config --add channels https://mirrors.tuna.tsinghua.edu.cn/anaconda/cloud/pytorch/
conda config --set show_channel_urls yes
```

### 中科大conda镜像

```python
conda config --add channels https://mirrors.ustc.edu.cn/anaconda/pkgs/main/
conda config --add channels https://mirrors.ustc.edu.cn/anaconda/pkgs/free/
conda config --add channels https://mirrors.ustc.edu.cn/anaconda/cloud/conda-forge/
conda config --add channels https://mirrors.ustc.edu.cn/anaconda/cloud/msys2/
conda config --add channels https://mirrors.ustc.edu.cn/anaconda/cloud/bioconda/
conda config --add channels https://mirrors.ustc.edu.cn/anaconda/cloud/menpo/
conda config --set show_channel_urls yes
```

## `~/.condarc`文件

`~/.condarc`配置文件可以用来自定义Conda的行为。`~/.condarc`是一个文本文件，它存储了Conda的配置选项，位于主目录下（通常是`/home/username`或`/Users/username`）。

### 文件格式

`~/.condarc`文件使用YAML（YAML Ain't Markup Language）格式编写。YAML是一种人类可读的数据序列化格式，它使用缩进和键值对的方式表示数据。

下面是一个示例`~/.condarc`文件的基本格式：

```yaml
key1: value1
key2: value2
```

其中，`key1`和`key2`是配置选项的键，`value1`和`value2`是相应选项的值。可以根据需要添加或编辑键值对来自定义配置。

### 常见配置选项

- `channels`：指定Conda的频道列表，用于查找和安装软件包。
- `custom_channels`：自定义频道。
- `channel_priority`：指定Conda频道的优先级，可以是`strict`（严格模式，始终使用最高优先级频道）或`flexible`（灵活模式，根据软件包可用性选择频道）。
- `create_default_packages`：指定在创建新环境时默认安装的软件包列表。
- `default_channels`：指定默认的Conda频道列表。
- `envs_dirs`：指定Conda环境的存储目录。
- `pkgs_dirs`：指定Conda的package储存目录。
- `ssl_verify`：指定是否验证SSL证书。
- `proxy_servers`：指定代理服务器的配置。

可以根据需要在`~/.condarc`文件中添加和编辑这些选项，或者根据Conda文档中提供的其他选项进行配置。

### 示例

`~/.condarc`文件：

```yaml
channels:
  - conda-forge
  - defaults
channel_priority: strict
default_channels:
  - https://default_channels.example.com
custom_channels:
  - https://mychannel.example.com
create_default_packages:
  - numpy
  - pandas
envs_dirs:
  - /home/username/conda/envs
pkgs_dirs:
  - /home/username/conda/pkgs
ssl_verify: true

proxy_servers:
  http: http://proxy.example.com:8080
  https: https://proxy.example.com:8080
```

示例文件指定了两个频道（`conda-forge`和`defaults`），设置了严格的频道优先级，指定了默认安装的软件包（`numpy`和`pandas`），设置了环境存储目录，启用了SSL证书验证，并配置了代理服务器。

在`~/.condarc`文件中的选项是可以根据需求进行自定义。可以根据自己的环境和偏好进行相应的配置。

在安装软件包时，Conda会按照以下顺序搜索频道：

1. 首先，会按照`channels`部分中定义的顺序搜索默认频道（如`defaults`）。
2. 然后，会按照`channels`部分中定义的顺序搜索其他频道。
3. 最后，如果软件包在默认频道和其他频道中都不存在，Conda会搜索`custom_channels`部分中定义的自定义频道。

当使用Conda安装软件包时，它会按照以下顺序搜索软件包：

1. 首先，Conda会检查`pkgs_dirs`部分中定义的目录，按照它们在`pkgs_dirs`中的顺序进行搜索。如果软件包在这些目录中找到了，Conda将直接使用它们，而不会继续搜索其他频道。
2. 如果软件包在`pkgs_dirs`中没有找到，Conda会按照`channels`部分中定义的顺序搜索频道，包括默认频道和其他频道。
3. 如果软件包在默认频道和其他频道中都不存在，Conda会搜索`custom_channels`部分中定义的自定义频道。

## Jupyter Notebook

安装：

```python
conda install jupyter
```

或者**ANACONDA.NAVIGATOR**界面下直接安装。

------

### nb_conda_kernels

`nb_conda_kernels` 是一个 Jupyter Notebook 插件，它许你在 Jupyter Notebook 中切换使用不同的 Conda 环境作为内核。

当你安装并启用 `nb_conda_kernels` 插件后，你可以在 Jupyter Notebook 的内核选择器中看到来自于所有已安装的 Conda 环境的内核选项，而不仅限于 "base" 环境。这样，你可以轻松地在 Jupyter Notebook 中切换使用不同的 Conda 环境来运行代码。

通过选择特定的 Conda 环境内核，你可以确保在 Jupyter Notebook 中使用该环境的 Python 解释器和已安装的包。这对于在不同的环境中进行开发和测试非常方便，每个环境都有自己独立的依赖和设置。

所以，`nb_conda_kernels` 插件使得在 Jupyter Notebook 中切换使用不同的 Conda 环境作为内核变得更加容易。

使用方法：

```python
conda activate my-conda-env   
conda install ipykernel #如果该环境没有jupyter的话，需要安装该模块
conda deactivate
conda activate base      
conda install nb_conda_kernels
jupyter notebook
```

安装好后，打开 jupyter notebook 就会显示所有的 conda 环境，点击便可以随意切换。

------

### jupyter_contrib_nbextensions

安装插件到当前用户：

```python
pip install jupyter_contrib_nbextensions 
jupyter contrib nbextension install --user
```

安装和启用Jupyter Notebook 的 nbextensions 配置插件：

```python
pip install jupyter_nbextensions_configurator
jupyter nbextensions_configurator enable --user
```

这个包提供了一个配置界面，可以方便地启用和禁用 Jupyter Notebook 的各种扩展插件。

`jupyter nbextensions_configurator enable --user` 是用于启用 Jupyter Notebook 的 nbextensions 配置插件。这个命令会在当前用户的 Jupyter Notebook 环境中启用 nbextensions 配置插件，使你可以通过浏览器访问配置界面。

安装完成以后，打开 **jupyter notebook** 可以看到菜单栏多了 ：**Nbextensions** 一栏，这样就可以安装插件了，推荐三个常用的插件：**Table of Contents**， **Variable Inspector** 和 **ExcecuteTime**。

1. **Table of Contents（目录）**: Table of Contents插件允许你自动生成笔记本的目录。它会扫描笔记本中的标题，并创建一个可导航的目录结构，使你能够快速跳转到感兴趣的部分。这对于长篇笔记本或包含大量内容的笔记本非常有用，可以提高浏览和导航的效率。
2. **Variable Inspector（变量查看器）**: Variable Inspector插件提供了一个方便的界面，用于查看和管理当前Jupyter Notebook中的变量。它会显示你在笔记本中定义的变量列表，并显示它们的名称、类型和值。这对于调试代码、了解当前环境中的变量状态以及快速检查变量内容非常有用。
3. **ExecuteTime（执行时间）**: ExecuteTime插件可以记录每个代码单元格的执行时间。当你运行一个代码单元格时，它会显示代码的执行时间，让你知道每个单元格花费了多长时间执行。这对于性能优化、代码调试和评估代码运行时间非常有用。

除此之外，其他常用的有：

- **Codefolding：** 代码折叠
- **Autopep8：** 自动代码格式优化
- **AutoSaveTime：** 控制脚本的自动保存时间
- **Hide Input All：** 隐藏所有的代码单元，保持所有的输出和 markdown 单元可见
- **Spellchecker：** 对 markdown 单元中的内容进行拼写检查

### 指定启动目录

#### 方法一：修改配置文件

1. 生成配置文件

   ```python
   jupyter notebook --generate-config
   ```

   命令执行后，会显示配置文件的路径，一般为用户目录下的.jupyter文件夹。

2. 修改配置文件

   找到配置文件中的`#c.NotebookApp.notebook_dir = ''`

   去掉注释，添加指定的目录：

   `c.NotebookApp.notebook_dir =  'r'e:/testpath''`

   此处路径要考虑字符转义。

#### 方法二：修改快捷图标的目标位置

也可以在快捷方式图标中的属性出修改，将目标位置下最后的引号的内容替换为你的路径。

#### 方法三：指定单次启动位置

`jupyter notebook --notebook-dir=` 是Jupyter Notebook 的命令行选项之一，用于指定启动 Jupyter Notebook 时的默认笔记本目录。

当你在命令行中执行 `jupyter notebook` 命令时，Jupyter Notebook 会在默认的笔记本目录中打开一个服务器，并在浏览器中显示 Jupyter Notebook 的界面。默认情况下，该目录是当前工作目录。

通过使用 `--notebook-dir=` 选项，你可以指定一个自定义的笔记本目录。

这样，Jupyter Notebook 将在指定的目录中打开服务器，并在浏览器中显示该目录下的笔记本文件。

使用 `--notebook-dir=` 选项可以方便地切换默认的笔记本目录，适应不同的工作环境和笔记本存储位置。