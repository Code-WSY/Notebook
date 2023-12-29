# 远程部署Jupyter

## 安装

安装`jupyter`：

```bash
pip install jupyter
```

## 设置密码

设置登录密码：

```python
jupyter notebook password
#或者jupyter lab password 两者一样

Enter password:  ****  # 以后可直接通过这个密码 login
Verify password: ****
[NotebookPasswordApp] Wrote hashed password to /Users/you/.jupyter/jupyter_server_config.json

```

密码会保存至 `~/.jupyter/jupyter_server_config.json` 文件，其他配置信息也可以在上面进行输入。

注意，大部分教程中，对于将密码转为哈希密码依旧是用的`notebook.auth`模块，

**但在新版本中已经转到了`jupyter_server.auth`模块中：**

```python
(jupyter_web) jupyter@suyunubuntu:~/wsy$ ipython
In [1]: from jupyter_server.auth import passwd
In [2]: passwd()
Enter password:  *** #输入你的密码
Verify password: *** #输入你的密码
Out[2]: 'argon2:$argon2id$v=19$m=10240,t=10,p=8$ZBADGlxKb/l2h1HR1VaGkw$Jn3R8lt8QaSncyx1norwkMYdtnCLZl0cNYMRk7brWO8'
```

官方参考路径：

- Windows: `C:\Users\USERNAME\.jupyter\jupyter_notebook_config.py`
- OS X: `/Users/USERNAME/.jupyter/jupyter_notebook_config.py`
- Linux: `/home/USERNAME/.jupyter/jupyter_notebook_config.py`

------

## 配置

在之前的Python 文件中，这些特征将具有前缀例如`c.ServerApp`. 

配置文件可能如下所示：

```python
# inside a jupyter_server_config.py file.
c.ServerApp.port = 8888
```

`JSON`中的相同配置如下所示：

```json
{
    "ServerApp": {
        "port": 8888
    }
}
```

添加外部`IP`能够进行访问的配置：

```json
{
  "ServerApp": {
    "ip":"*"
    ,"port":8888 
    ,"open_browser":false
    ,"password": "刚刚生成的部分，也在这个文件中"
  }
}

```

端口默认是8888，但是需要确认该端口是否开放。

还可以设置根目录地址：

```json
  {
  "NotebookApp": {
    "notebook_dir":"你希望设置的更目录地址"
  }
  }
```

## 后台运行

通过以下命的末尾添加 `&`在后台运行服务，再通过`nohup` 命令，**ssh窗口关闭后仍能正常访问：

```bash
nohup jupyter-lab &
#或者进行输出重定向
nohup  jupyter-lab > output.txt 2>&1 & #全部输出到output.txt
nohup  jupyter-lab > output.txt 2>error.txt & #错误输出到error.txt
```

浏览器通过 `服务器公网IP:端口`便可以进行访问。

## 关闭进程

查找进程的pid：

```shell
ps -aux | grep jupyter
#或者：pgrep jupyter
```

关闭：

```shell
kill -9 pid
```

> **注意：如果在云服务器上部署jupyter（如阿里云），要记得在安全组上打开入站端口。**
