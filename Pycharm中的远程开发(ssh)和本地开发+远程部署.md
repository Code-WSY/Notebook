# Pycharm中的远程开发和部署

## 远程开发(SSH)

远程开发主要是基于JetBrains Gateway启动器。

界面

<img src="http://sy0316.oss-cn-hangzhou.aliyuncs.com/img/image-20231224034355777.png" alt="image-20231224034355777" style="zoom: 50%;" />

这是一个轻量级启动器，可将远程服务器与本地计算机连接起来，在后端下载必要的组件，并在 JetBrains Client 中打开项目。 JetBrains Gateway 可以用作独立启动器，也可以将其用作 IDE 的入口点，以连接到远程服务器。

目前仅支持Linux

<img src="http://sy0316.oss-cn-hangzhou.aliyuncs.com/img/image-20231224034422818.png" alt="image-20231224034422818" style="zoom:50%;" />

远程开发（SSH）的操作较为简单，具体操作参见官方文档([连接并使用 JetBrains Gateway |IntelliJ IDEA 文档](https://www.jetbrains.com/help/idea/remote-development-a.html#gateway))。

## 本地开发-部署

IntelliJ IDEA 假定所有开发、调试和测试都在本地的计算机上完成，然后将代码部署到生产环境。

坚持这种“本地开发-部署”模式的原因在于 IntelliJ IDEA 提供代码补全、代码检查、代码导航和其他编码辅助功能的方式。所有这些功能都基于项目文件索引，IntelliJ IDEA在加载项目时会构建该索引，并在编辑代码时动态更新。

部署的具体操作参见官方文档[Deployment | IntelliJ IDEA Documentation (jetbrains.com)](https://www.jetbrains.com/help/idea/deploying-applications.html)

### 自定义上传和下载

- 若要在上载和下载过程中跳过特定文件或整个文件夹，在“按名称排除项目”字段中，指定定义这些文件和文件夹名称的模式。

  使用分号作为分隔符，使用星号匹配零个或多个字符，使用问号匹配单个字符。`; * ?`

  例如，如果文件夹**样式表**包含三个文件 style.css、style1.css 和 style2.scss，则排除整个文件夹、排除 style.css 以及 style1.css 和 style2.scss。可以用下面三种表达：`style*  style?.css  style?.*`

  从 [Regular-Expressions.info](https://www.regular-expressions.info/quickstart.html) 可以了解更多信息。

  排除以递归方式应用。这意味着，如果匹配的文件夹具有子文件夹，则也不会部署这些子文件夹的内容。

  有关详细信息，请参阅[从上传和下载中排除文件和文件夹](https://www.jetbrains.com/help/idea/excluding-files-and-folders-from-deployment.html)。

- 通过选中或清除相应的复选框来指定上载和下载过程的详细信息。

### 部署的注意事项

#### 同步环境

1. **创建虚拟环境**

   首先通过conda激活一个母环境：

   ```shell
   conda activate myenv
   ```

   接着基于此环境在此创建项目的虚拟环境：

   ```shell
   python -m venv venv
   ```

   最后，激活创建的虚拟环境：

   `python -m venv venv` 是一个在命令行中创建Python虚拟环境的命令。这里的`-m`是告诉Python去执行一个模块，而不是直接运行一个脚本。具体来说，这个命令做了以下几件事情：

   1. `python`：调用Python解释器。
   2. `-m venv`：指定要运行的模块是`venv`，这是Python标准库中用于创建和管理虚拟环境的模块。
   3. `venv`：这是你想要创建的虚拟环境的名称。在这个例子中，虚拟环境的名称也是`venv`。

   执行这个命令后，Python会在当前目录下创建一个名为`venv`的文件夹，这个文件夹包含了一个独立的Python环境，你可以在这个环境中安装不同的Python包，而不会影响到全局的Python环境。这样，你可以为不同的项目创建不同的虚拟环境，每个环境都可以有自己独立的依赖库。

   - 在 Windows上，运行：

     ```powershell
     .\venv\Scripts\activate
     ```

   - 在 Linux 或 macOS上，运行：

     ```shell
     source venv/bin/activate
     ```

   当虚拟环境被激活后，你的命令提示符通常会显示环境的名称（在本例中是 `venv`）。

   

2. **同步虚拟环境依赖**

   为了方便导出这个虚拟环境，以便在其他地方配置，可以使用 `pip` 工具来生成一个包含所有已安装包的要求文件（通常称为 `requirements.txt`）。这个文件可以在另一个环境中被用来重建相同的虚拟环境，包括所有依赖包及其精确版本。以下是步骤：

   1. **激活你的虚拟环境**：首先，确保你的虚拟环境是激活的。根据你的操作系统，使用之前提到的 `activate` 命令。

   2. **导出依赖列表**：在虚拟环境中，使用以下命令导出所有已安装包的列表：

      ```shell
      pip freeze > requirements.txt
      ```

      这个命令会创建一个 `requirements.txt` 文件，其中包含了虚拟环境中所有包及其版本的列表。

   3. **在新环境中重建虚拟环境**：在另一个系统或位置，可以使用这个 `requirements.txt` 文件来创建一个具有相同依赖的新虚拟环境。(在部署的时候，即可通过此办法同步环境中的依赖)

   4. **安装依赖**：在新的虚拟环境中，运行以下命令安装 `requirements.txt` 文件中列出的所有包：

      ```shell
      pip install -r requirements.txt
      ```

      这将安装与原始环境相同的依赖包和版本。

   通过这种方法，你可以确保在不同的机器或环境中具有一致的开发环境，这对于确保代码的可移植性和一致性非常重要。记得将 `requirements.txt` 文件添加到你的项目源代码管理中，例如Git，这样其他开发人员或自动化系统也可以使用它。

