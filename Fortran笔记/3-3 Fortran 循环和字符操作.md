# 3 Fortran 循环和字符操作



## 3.1 控制结构：循环

循环结构有两种基本形式：**当循环**和**迭代循环**(或计数循环)。

当循环中的代码循环次数不确定，直到满足了某个用户指定的条件为止。 

迭代循环中的代码循环次数是确定的，重复次数在循环开始前是已知的。

### 3.1.1 当循环

一般形式：

```fortran
DO
...
IF(逻辑表达式)EXIT
...
END DO
```

在DO和END DO之间的语句块不确定地重复执行，知道逻辑表达式变为真，执行了EXIT语句。EXIT语句执行后，控制程序转到END DO之后地第一条语句处。

源代码：统计分析

```fortran
PROGRAM STATS_1
!求平均值和标准方差
IMPLICIT NONE
!数据字典：声明变量类型，定义和计量单位
INTEGER::n=0          !输入样本个数
REAL::std_dev=0       !样本的标准差
REAL::sum_x=0         !
REAL::sum_x2=0
REAL::x=0
REAL::x_bar
!循环计数
DO
READ(*,*)'Enter the number:',x
WRITE(*,*)"The number is:",x
IF(x<0) EXIT
n=n+1
sum_x=sum_x+x
sum_x2=sum_x2+x**2
END DO
x_bar=sum_x/n
xtd_dev=SQRT((n*sum_x2-(sum_x)**2)/(n*(n-1)))
WRITE(*,*)"The mean this data set is: ",x_bar
WRITE(*,*)"The standard deviation is：",std_dev
WRITE(*,*)"The number of data points is：",n
END PROGRAM STATS_1

```

### 3.1.2 DO  WHILE 循环

 **DO WHILE**结构的形式如下：

```fortran
DO WHILE(逻辑表达式)
...
...
...
END DO
```

在新程序中不要使用DO WHILE循环，而使用更一般的当循环。

### 3.1.3 迭代或技术循环

计数循环结构：

```fortran
DO index=istart,iend,incr
statement_1
statement_2
...
statement_n
END DO
```

$\bold{index}$是一个整型变量，作为循环计数器使用。整型数$\bold{istart、iend、incr}$是计数循环的参数；它们控制变量$\bold{index}$在执行旗期间的数值。参数$\bold{incr}$是可选的(默认为1，可设置为负数)。

计数循环结构的作用如下

1. 三个循环参数$\bold{istart、iend、incr}$可以是常量、变量或表达式。如果是变量或表达式，其值是在循环开始前进行计算，得到的数值用于控制循环。

2. 在$\bold{DO}$循环执行的开始处，程序将数值$\bold{istart}$赋给控制变量$\bold{index}$​。如果$\bold{index*incr\leq iend*incr}$，程序执行循环体内的语句。

3. 在循环体内的语句被执行后，控制变量重新计算为：

   ```fortran
   index = index+incr
   ```

   如果$\bold{index*incr\leq iend*incr}$，程序执行循环体内的语句。

4. 只要$\bold{index*incr\leq iend*incr}$，第2步就反复执行。

> 统计分析：实现一个算法，读取一组测量值，计算输入数据集的平均值和标准方差，数据集中的数值可以为正数、 负数或零。
>
> （由于这次不能使用一个数据值作为结束标记， 所以要求用户告知输入数值的个数， 然后使用DO循环读入这些数值。）

```fortran
PROGRAM STATS_2
IMPLICIT NONE
INTEGER::n     !输入样本个数
INTEGER::i     !计数器
REAL::x        !样本数据
REAL::X_bar    !平均值
REAL::std_dev  !方差
REAL::sum_1
REAL::sum_2 
WRITE(*,*)'Enter number of points:'
READ(*,*),n
IF(n<2)THEN
	WRITE(*,*)'At least 2 values must be entered.'
ELSE
	DO i=1,n
		WRITE(*,*)'Enter the number:'
		READ(*,*),x
		sum_1=sum_1+x
		sum_2=sum_2+x**2
	END DO
	x_bar=sum_1/n
	std_dev=SQRT((n*sum_2-(sum_1)**2)/(n*(n-1)))
END IF
WRITE(*,*)"The mean this data set is: ",x_bar
WRITE(*,*)"The standard deviation is：",std_dev
WRITE(*,*)"The number of data points is：",n
END PROGRAM STATS_2

```

### 3.1.4 CYCLE和EXIT语句

类比：continue和break

**CYCLE：**

```fortran
PROGRAM test_cycle
INTEGER::i
DO i=1,5
IF(i==3) CYCLE
WRITE(*,*)i
END DO
WRITE(*,*)'END OF LOOP!'
END PROGRAM test_cycle
```

**输出：1，2，4，5**

**EXIT：**

```fortran
PROGRAM test_cycle
INTEGER::i
DO i=1,5
	IF(i==3) EXIT
	WRITE(*,*)i
END DO
WRITE(*,*)'END OF LOOP!'
END PROGRAM test_cycle
```

**输出：1，2**

### 3.1.5 命名的循环

一般形式：

```Fortran
[name:]DO
	statement
	...
	IF(逻辑表达式)CYCLE[name]
	...
	IF(逻辑表达式)EXIT[name]
	END DO[name]
	!
[name:]DO index=istart,iend,incr
	statement
	...
	IF(逻辑表达式)CYCLE[name]
	...
	IF(逻辑表达式)EXIT[name]
	END DO[name]
```

### 3.1.6 嵌套循环和IF块结构

嵌套循环：

```fortran
PROGRAM nested_loops
INTEGER::i,j,product
DO i =1,3
	DO i=1,3
		product =i*j
		WRITE(*,*)i,"*",j,"=",product
	END DO
END DO
END PROGRAM nested_loops
```

## 3.2 字符赋值和字符操作

### 3.2.1  字符赋值

```fortran
CHARACTER(len=3)::file_ext
file_ext='f'
```

如果字符表达式短于它所赋值的字符变量的长度，那么变量的多余部分就用空格填充。

如果字符表达式长于它所赋值的字符变量的长度，那么超出字符变量的部分就被省略。

### 3.2.2  子串提取

子串（子字符串）提取选择字符变量的一部分，并将该部分看作一个独立的字符变量。

形式：

```fortran
PROGRAM test_charl 
CHARACTER(len=8) ::a,b,c 
a='ABCDEFGHIJ' 
b='12345678' 
C=a(5:7) 
b(7:8)=a(2:6) 
END PROGRAM test_charl
```

### 3.2.3 连接(//)操作符

可以将两个或者多个字符串或子串合并成一个大的字符串。这个操作成为连接。

```fortran
PROGRAM test_char2 
CHARACTER (len=10)::a
CHARACTER(len=8)::b,c 
a ='ABCDEFGHIJ' 
b ='12345678' 
c =a(l:3)//b(4:5)//a(6:8) 
END PROGRAM test_char2 
!变狱c包含字符串'ABC45FGH'。

```

### 3.2.4 字符数据的关系运算符

利用关系运算符**＝＝、／＝、＜、＜＝、＞**和**＞＝**，字符串可以在逻辑表达式中进行比较。比较的结果为逻辑值**真**或**假**。

1. 单个字符是基于执行程序的计算机上的字符排序序列。字符排序序列是字符在特定的字符集内出现的顺序($\bold{ASCII}$码)。

2. 字符串比较是比较从每个字符串的第一个字符开始。如果它们是相同的，那么再比较第二个字符

   例如：**'AAAAAB'>'AAAAAA'**。

3. 如果两个字符串的长度不同,比较操作从每个字符串的第一个字符开始，每个字符进行比较，直到发现了4为止。如果两个字符串一直到其中一个结束时始终是相同，那么就认为另一个字符串为大。

   例如：**'AB'>'AAAA' and 'AAAAA'>'AAAA'**

### 3.2.5 内置字符函数

|    函数名和参数    | 参数类型 | 结果类型 | 注释                                        |
| :----------------: | :------: | -------- | ------------------------------------------- |
|  **ACHAR(ival)**   | **INT**  | **CHAR** | 返回**ival**在**ASCII**排序序列中相应的字符 |
|  **IACHAR(char)**  | **CHAR** | **INT**  | 返回**char**在**ASCII**排序序列中相应的整数 |
|   **LEN(str1)**    | **CHAR** | **INT**  | 返回**str1**的字符长度                      |
| **LEN_TRIM(str1)** | **CHAR** | INT****  | 返回**str1**的长度，不包括尾部的空格        |
|   **TRIM(str1)**   | **CHAR** | **CHAR** | 返回被截去尾部空格的**str1**                |
