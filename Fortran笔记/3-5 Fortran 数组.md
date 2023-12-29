# 5 数组

## 5.1 声明数组

在使用数组之前， 它包含的**元素类型**和**个数**必须**用类型声明**语句来**向编译器声明**，以便编译器知道要存储在数组中的数据类型，以及需要多少内存来存储数组。

```fortran
REAL,DIMENSION(15)::voltage
!DIMENSION:说明被定义数组的大小
CHARACTER(len=20),DIMENSION(50)::last_name
!如上，有50个元素，每个元素为20个字符长度变量的数组
```

- **数组常量**：完全由常量组成的数组。通过将**常量数值**放在**特殊的分隔符**之间来定义数组常量，这种特殊分隔符称为**数组构造器(array constructor)**

- **数组构造机**：起始分隔符**( ' (/ ' or ' [ ' )**，结束分隔符**( ' /) ' or ' ] ')**

  

## 5.2 在Fortran语句中使用数组元素

### 5.2.1 数组也是普通变量

```Fortran
INTEGER, DIMENSION(10)::index
REAL,DIMENSION(3)::temp
####
index(1)=5
temp(3)=REAL(index(1))/4.
WRITE(*,*)'index(1)= ', index(1)
```

在某些情况下，整个数组和部分数组可以用在表达式和赋值语句中。

### 5.2.2 数组元素初始化

1. **用赋值语句初始化数组**

   **采用DO循环**：

```fortran
REAL,DIMENSION(10)::array1
INTEGER::i
DO i=1,10
    array1(i)=REAL(i)
END DO
```

   **采用数组常量：**

```fortran
REAL,DIMENSION(10)::array1
array1=[1.,2.,3.,4.,5.,6.,7.,8.,9.,10.]
```

   **直接用赋值语句将所有元素初始化为同一个值：**

```fortran
REAL,DIMENSION(10)::array1
array1=0.
```

2. **在类型声明语句中初始化数组**

```fortran
INTEGER,DIMENSION(5)::array2=[1,2,3,4,5]
```

   **利用隐式DO循环生成数组：**

   隐式DO循环的常规形式：

```fortran
[(arg1,arg2,...,index = istart,iend,incr)]
READ(unit,format)(arg1,arg2,...,index = istart,iend,incr)
WRITE(unit,format)(arg1,arg2,...,index = istart,iend,incr)
!arg1,arg2:每次循环时，所求的值
```

   于是可以：

```fortran
INTEGER,DIMENSION(200)::array2=[(i,i=1,200)]
```

   利用隐式DO循环与常量嵌套混合生成数组：

```fortran
INTEGER,DIMENSION(25)::array2=[((i,i=1,4),6*j,j=1,5)]
!生成数组
j=1: 1 2 3 4 6
j=2: 1 2 3 4 12
j=3: 1 2 3 4 18
j=4: 1 2 3 4 24
j=5: 1 2 3 4 30

```

3. **用READ语句初始化数组**

### 5.2.3 改变数组下标的取值范围

格式：

```fortran
REAL,DIMENSION(Lower_bound:Upper_bound)::array
!如,同样是五元素数组：
REAL,DIMENSION(5)::a1
REAL,DIMENSION(-2,2)::b1
REAL,DIMENSION(5,9)::c1
```

### 5.2.4 数组下标越界

建议：

在程序开发和调试时，总是打开Fortran编译器的边界检测选项，以帮助捕获产生越界引 用的编程错误。 如果必要， 在最终程序中可以关闭边界检测选项， 以提高执行速度。

#### 5.2.5 在数组声明中使用命名常数

通过设置命名常数，很容易去改变程序中数组的大小。

```fortran
INTEGER,PARAMETER::Max_size=1000
REAL::array1(Max_size)
REAL::array2(2*Max_size)
```

建议在Fortran程序中始终用参数声明数组的大小，以保证程序很容易修改

## 5.3 在Fortran语句中使用整个数组和部分数组

### 5.3.1 操作整个数组

1. 如果两个数组有相同的结构，那么可以对它们进行普通算术操作，操作会逐个应用于对应元素之间。这里只要求结构相同，下标值范围不一定要求相同。
2. 标量与数组是一致的。标量数值被平等地作用于数组的每个元素上。
3. 许多用于标量数值上的Fortran内置函数也可能以数组作为输入参数，返回数组作为结果。返回数组将含有逐个元素地应用函数道输入数据而产生地结果。这些函数为基本内置函数，包括(ABS, SIN, COS, EXP, LOG 等。)

### 5.3.2 操作部分数组

部分数组用**下标三元组**或**向量下标**来代替数组下标来指定。

**下标三元组**常见形式：

```fortran
subscript_1:subscript_2:stride
!下列内容都是合法三元组：
subscript_1:subscript_2
subscript_1:
subscript_1::stride
:subscript_2
:subscript_2:stride
::stride
:
!缺省subscript_1:默认取值为数组中第一个元素下标
!缺省subscript_2:默认取值为数组中最后一个元素下标
!缺省stride:默认值取1
```

**向量下标**常见形式：

```fortran
INTEGER,DIMENSION(5)::vec = [1,6,4,1,9]
REAL,DIMENSION(10)::a = [1,-2,3,-4,5,-6,7,-8,9,-10]
!a(vec)=[1,-6,-4,1,9]
```

通过向量下标也可**指定数组元素**：

```fortran
INTEGER,DIMENSION(5)::vec = [1,3,2,5,4]
REAL,DIMENSION(10)::a = [1,-2,3,-4,5]
REAL,DIMENSION(5)::b
b(vec)=a
```

## 5.4 输入和输出

### 5.4.1 数组元素的输入和输出

```fortran
WRITE(*,100)a(1),a(2),a(3),a(4),a(5)
100 FORMAT('a = ',5F10.2)
```

### 5.4.2 隐式DO循环

常规格式：

```fortran
WRITE(*,100)(a(i),i=1,5)
100 FORMAT('a = ',5F10.2)
```

嵌套式隐式DO循环：

```fortran
WRITE(*,110)((i,j,i=1,3),2*j,j=1,3)
!对外层循环中的每一步，都完全执行一遍内层循环。
```

**标准DO循环I/O和隐式DO循环I/O区别：**

标准DO循环：

```fortran
INTEGER,DIMENSION::arr=[1,2,3,4,5]
DO i=1,5
	WRITE(*,1000)arr(i),arr(i)*2,arr(i)*3
	1000 FORMAT(6I6)
END DO

```

等价于：

```fortran
INTEGER,DIMENSION::arr=[1,2,3,4,5]	
WRITE(*,1000)arr(1),arr(1)*2,arr(1)*3
WRITE(*,1000)arr(2),arr(2)*2,arr(2)*3
WRITE(*,1000)arr(3),arr(3)*2,arr(3)*3
WRITE(*,1000)arr(4),arr(4)*2,arr(4)*3
WRITE(*,1000)arr(5),arr(5)*2,arr(5)*3
1000 FORMAT(6I6)
!OUTPUT:
!1 2 3
!2 4 6
!3 6 9
!4 8 12
!5 10 15
```

隐式DO循环：

```fortran
INTEGER,DIMENSION::arr=[1,2,3,4,5]
	WRITE(*,1000)(arr(i),2*arr(i),3*arr(i),i=1,5)
	1000 FORMAT(6I6)
```

等价于：

```fortran
INTEGER,DIMENSION::arr=[1,2,3,4,5]
	WRITE(*,1000)arr(1),2*arr(1),3*arr(1),&
				 arr(2),2*arr(2),3*arr(2),&
				 arr(3),2*arr(3),3*arr(3),&
				 arr(4),2*arr(4),3*arr(4),&
				 arr(5),2*arr(5),3*arr(5)
	1000 FORMAT(6I6)
!OUTPUT:
!1 2 3 2 4 6
!3 6 9 4 8 12
!5 10 15

```

> 如果在READ语句变量全赋完值之前扫描到了格式的结尾，则程序丢弃当前的输入缓冲区，然后重新获取一个新的输入缓冲区，并在格式中**最右边不带重复次数**的**开始括号处**重新开始，WRITE同样如此。

## 5.5 小结

1. 数组类型声明语句：

```fortran
type,DIMENSION([i1:]i2)::array1,...
!如:
REAL,DIMENSION(100)::array
INTEGER,DIMENSION(-5:5)::i
```

2. 隐式DO循环结构：

```fortran
READ(unit,format)(argl,arg2,...,index = istart,iend,incr)
WRITE(un.1.t,format)(argl,arg2,...,index = istart,iend,incr)
[(array1,array2,...,index=istart,iend,incr)]
!如：
WRITE (*,*)(array(i),i = 1,10 ) 
INTEGER,DIMENSION(lOO) ::values 
values = [(i,i=l,100)] 
```
