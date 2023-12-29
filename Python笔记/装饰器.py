import time
def time_decorator(func):
    #用来替代func函数的引用
    def wrapper(*args, **kwargs):
        start_time = time.time()
        result = func(*args, **kwargs)
        end_time = time.time()
        print("函数 {} 执行时间：{} 秒".format(func.__name__, end_time - start_time))
        return result+1
    # -------------------------------------------------------------------------------- #
    print("装饰器定义结束")
    print("=====================================")
    return wrapper

# -------------------------------------------------------------------------------- #
"""

def func(NUM):
    print("我是func函数")
    print(func.__name__)
    return NUM+1
func = time_decorator(func) #将func函数的引用指向wrapper函数
"""

# -------------------------------------------------------------------------------- #
#等效于：
@time_decorator
def func(NUM):
    print("我是func函数")
    print(func.__name__)
    return NUM+1


# -------------------------------------------------------------------------------- #
# 调用被装饰的函数
A=func(10)
