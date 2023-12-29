# 1 Fortran 基础知识

## 1.1 Fortran字符集

Fortran语言使用的特殊字母表被称为Fortran字符集。目前Fortran字符集由97个字符组成：

| 符号个数 | 类型         | 取值                                         |
| -------- | ------------ | -------------------------------------------- |
| 26       | 大写字母     | A~Z                                          |
| 26       | 小写字母     | a~z                                          |
| 10       | 数字         | 0-9                                          |
| 1        | 下划线       | _                                            |
| 5        | 算数符号     | + - * / **                                   |
| 28       | 其他各种符号 | ().+.'$: ! " % & ;<>?和空格 ~\[ ] ` #@和空格 |

Fortran对字母大小写不敏感。A和a是两个相同的字母。

## 1.2 Fortran语句结构

语句分类：可执行部分+不可执行部分。

$\&$符号：进行标记并在下一行继续这一行书写，直到结束。

$!$符号：注释说明符号。

语句标号：第四行，以数字开始，语句标号可以是1~99999中任何一个数字，是Fortran语句中的“名字”，使用它可以在程序的其他地方引用这条语句。标号数字在程序单元中必须是唯一的。

```fortran
output = input1+input2  !求和输入值
output = input1 &
		+input2        !求和输入值
999 output = input1 &   !求和输入值
			&+input2
```

## 1.3 Fortran程序结构

```fortran
PROGRAM my_first_program
!目的：
!    本程序主要说明Fortran语言的基本特点
!
!  声明程序中用到的变量
INTEGER::i,j,k  !所有变量均为整型
!  获取存入变量i和j的值
WRITE(*,*)'Enter the numbers to multiply'
READ(*,*)i,j
!求两个数的相乘
k = i * j
!输出计算结果
WRITE(*,*)'Result = ',k
!完成
STOP
END PROGRAM my_first_program
```

Fortran程序主要分为三个部分：

1. 声明部分：一组不可执行语句组成，位于程序开头($\boldsymbol{PROGRAM}$)，定义程序名(第一个字符必须是字母)和程序引用的数据及其变量的类型。如果存在PROGRAM语句，必须是程序的第一个语句行。
2. 执行部分：多条语句构成，描述程序完成的操作。
3. 终止部分：一条语句或终止程序执行的语句组成，告诉编译器程序结束。

### 1.3.1 声明部分

声明部分由不可执行语句组成，位于程序的开头，定义程序名和程序引用的数据以及变量的类型。

这一部分的第一条语句是PROGRAM语句。它对Fortran编译器指定程序的名字。Fortran的程序名可长达63个字符，还可以是字母、数字和下划线任意组合而成的字符串。但是，**程序名的第一个字符必须是字母**。如果存在PROGRAM语句，它必须是程序的第一个语句行。在这个例子中，程序被命名为my_first_program。

程序中的下面几行是注释，描述程序的作用。再下面跟随INTEGER类型声明语句，这条不可执行语句在本章后面有介绍。这里，它声明程序要用的整型变量I、J和K。

### 1.3.2 执行部分

执行部分由一或多条执行语句组成，描述程序将完成的操作。这个程序的第一条可执行语句是WRITE语句，它输出信息，提示用户键入两个待相乘的数据。下一条执行语旬READ语句，读入两个用户提供的整型数。第三条执行语句指示计
算机乘以两数1和j，结果存储在变量k。最后一条WRITE语句打印用户看到的结果。注释被嵌入在整个执行部分的任意位置。

### 1.3.3 终止部分

终止部分由STOP和END PROGRAM语句组成。**STOP语句告诉计算机停止运行。**END PROGRAM语句告诉编译器程序中不再有语句需要编译。

STOP语句格式有如下形式：

```fortran
STOP
STOP 3
STOP 'Error stop'
```

如果只使用STOP语句，则执行将停止。如果STOP语句与数字一起使用，则程序停止时将打印出该数字，通常将作为错误代码返回给操作系统。如果STOP语句与字符串一起使用，则程序停止时将打印出该字符串。

当STOP语句紧挨着出现在END PROGRAM语句之前，它是可选的；当到达END PROGRAM语句时，编译器将自动地产生一条STOP语句。

有一个替代版本的STOP语句叫做**ERROR STOP**。该版本停止程序，但它也通知操作系统程序无法正常执行。

```fortran
ERROR STOP 'Cannot access database'
```

此版本的STOP语句已在Fortran 2008中添加，如果需要通知操作系统，脚本程序异常失败，可能会很有用。

### 1.3.4 程序书写格式

一般Fonran编程原则：

1. 保留字都大写，如PROGRAM、READ和WRITE
2. 程序的变量用小写字母表示。
3. 名字中的下划线出现在两个字之间。
4. 大写字母作为常量名。

由于大写和小写字母在Fortran中作用相当，所以程序按任何一种方式来书写都可以。

## 1.4 常数与变量

### 1.4.1 Fortran中的常数与变量

Fortran常数是**数据对象**，它定义在**程序执行之前**，且在**程序执行期间取值不可改变**。当Fortran编译器遇到常数时，它将常数放置在一个位置已知的内存单元，无论何时程序使用常数，就**引用该存储位置。**

变量是一个**数据对象**，它的值在程序执行期间可以改变(Fortran变量的取值可以在程序执行前初始化，不初始化也行）。当Fortran编译器遇到变量时，它给变量**预留已知的内存单元**，无论何时程序使用变量，就**引用该存储位置**。

程序单元中的每个Fortran变量有唯一的名字，变量名是内存中特定位置的标号，该标号方便人类记忆和使用。Fortran中的变量名可以长达**63个字符**，由**字母**、**数字**和**下划线字符**的任意组合构成，但是名字的**第一个字符总必须是字母**。

在编写的程序的开头包含**数据字典**非常重要，数据字典列出了**程序中每个变量的定义**，定义含有两项内容：**数据项内容**和**数据项占用几个存储单元**的描述。写程序的时候，看上去数据字典可以是不必要的，但是当后期自己或其他人不得不修改程序时，它就价值无限。

Fortran有5个自带或“内置”的常数和变量数据类型：

- 数字类：**INTEGER,REAL,COMPLEX**
- 逻辑类：**LOGICAL**
- 字符类：**CHARACTER**

### 1.4.2 定义变量类型

1. #### **默认方式**

   任何以字母$i,j,k,l,m,n$开头的变量名假定为**INETEGER**，其他字母开头的变量名则假定为**REAL**。默认情况下没有变量的类型为字符型。

2. #### **显式定义**

   格式：

   ```fortran
   INTEGER:: var1[,var2, var3,... ]
   REAL:: var1[,var2,var3,... ]
   ```

   这里 [ ] 中的内容是可选的。在这种情况下，括号内的内容说明可以在一行中同时定义两个或多个变量，变量之间用逗号隔开。这些不可执行的语句称为类型声明语句。放在**PROGRAM语句之后，第一条可执行语句之前。**

   所有字符变量必须显式地用CHARACTER声明语句声明，格式：

   ```fortran
   CHARACTER(LEN=<len>):: var1[,var2,var3,... ]
   ```

   $<len>$​是变量中的字符数目，是可选的（默认为1）。假如圆括号中有数字，那么这个数字是语句声明的字符变量的长度。

## 15 赋值语句与算数运算

### 1.5.1 操作顺序

为了能明确地计算表达式，Fortran已经建立一系列规则来管理表达式中操作符的级别或计算顺序。

通常，Fortran遵循代数中的一般规则，这其中的运算操作计算顺序是：

1. 首先做圆括号内的计算，且内层括号比外层括号优先。
2. 再从右到左做指数运算。
3. 从左到右做乘法和除法运算。
4. 从左到右做加法和减法运算。

### 1.5.2 混合运算

含有实数和整数的表达式被称为混合模式的表达式，涉及实数和整数操作的运算称为混合模式运算。**在进行实数与**
**整数操作的情况下，计算机将整数转换为实数，然后进行实数运算，结果是实数类型。**

1. 当运算操作是在两个实型数据上完成，则结果的类型为REAL。
2. 操作是在两个整型数上执行，则结果是INTEGER。
3. 在进行实数与整数操作的情况下，结果是实数类型。

| 表达式 | 结果 |
| ------ | ---- |
| 1+1/4  | 1    |
| 1.+1/4 | 1    |
| 1+1./4 | 1.25 |

尽量避免混合模式表达。

### 1.5.3 Fortran五种转换函数

| 函数名和参数 | 参数类型 | 结果类型 | 返回值说明                |
| ------------ | -------- | -------- | ------------------------- |
| INT(X)       | REAL     | INTEGER  | X的整型部分(X被截尾）     |
| NINT(X)      | REAL     | INTEGER  | 接近X的整数(X被四舍五入） |
| CEILING(X)   | REAL     | INTEGER  | 大于或等于X的最小的整数值 |
| FLOOR(X)     | REAL     | INTEGER  | 小于或等于X的最大的整数值 |
| REAL(I)      | INTEGER  | REAL     | 整数转换为实数            |

## 1.6 内置函数

Fortran的部分常用内置函数：

| 函数名和参数 |   参数类型   | 结果类型 |                        说明                        |
| :----------: | :----------: | :------: | :------------------------------------------------: |
|   SQRT(X)    |     REAL     |   REAL   |                   大于0的平方根                    |
|    ABS(X)    | REAL/INTEGER |    *     |                     求X绝对值                      |
|   ACHAR(I)   |   INTEGER    | CHAR(I)  |            返回在ASCII表上I位置上的字符            |
|    SIN(X)    |     REAL     |   REAL   |              X的正弦(X必须是弧度值）               |
|   SIND(X)    |     REAL     |   REAL   |              X的正弦(X必x须是角度值）              |
|    COS(X)    |     REAL     |   REAL   |              X的余弦(X必须是弧度值）               |
|   COSD(X)    |     REAL     |   REAL   |              X的余弦(X必x须是角度值）              |
|    TAN(X)    |     REAL     |   REAL   |              X的正切(X必须是弧度值）               |
|   TAND(X)    |     REAL     |   REAL   |              X的正切(X必须是角度值）               |
|    EXP(X)    |     REAL     |   REAL   |                      e的X次幂                      |
|    LOG(X)    |     REAL     |   REAL   |                X的自然对数，其中X>0                |
|   LOGIO(X)   |     REAL     |   REAL   |               基数10的对数，其中X>O                |
|  IACHAR(C)   |   CHAR(I)    | INTEGER  |        返回字符C在ASCll表上对照顺序的位置值        |
|   MOD(A,B)   | REAL/INTEGER |    *     |                    模函数的余数                    |
|   MAX(A,B)   | REAL/INTEGER |    *     |                   A和B中的更大值                   |
|   MIN(A,B)   | REAL/INTEGER |    *     |                   A和B中的更小值                   |
|   ASIN(X)    |     REAL     |   REAL   |      X的反正弦,-1$\le$x$\le$1 (结果是弧度值)       |
|   ASIND(X)   |     REAL     |   REAL   |      X的反正弦,-1$\le$x$\le$1 (结果是角度值)       |
|   ACOS(X)    |     REAL     |   REAL   |      X的反余弦,-1$\le$x$\le$1 (结果是弧度值)       |
|   ACOSD(X)   |     REAL     |   REAL   |      X的反余弦,-1$\le$x$\le$1 (结果是角度值)       |
|   ATAN(X)    |     REAL     |   REAL   |   x的反正切,$-\pi/2\le x\le\pi/2$（结果是弧度值)   |
|   ATAND(X)   |     REAL     |   REAL   |      x的反正切,$-90\le x\le90$（结果是角度值)      |
|  ATAN2(Y/X)  |     REAL     |   REAL   | x四象限的反切函数$-\pi\le x\le\pi$（结果是弧度值） |
| ATAN2D(Y,X)  |     REAL     |   REAL   | x四象限的反切函数$-180\le x\le180$（结果是弧度值） |

​	

## 1.7 表控输入和输出语句

### 1.7.1 输入语句(READ)

```fortran
READ(*.*)input_list
```

$\boldsymbol{input\_list}$：读入的值放置在里面的变量列表。如果列表中有多个变量，它们用逗号分隔。

$\boldsymbol{(＊,＊)}$：含有读入操作的控制信息。

圆括号的第一数据域：指明从哪个**输入／输出单元**（或io单元）读入数据（输入／输出单元）。这个域中的**星号**意味着数据是从**计算机的标准输入设备上读入**，通常在交互模式下是键盘。

圆括号的第二个数据域：指明读入**数据的格式**。这个域的星号意味着使用**表控输入**（有时被称为**自由格式**输入）。

$\boldsymbol{list-directed input}$(表控输入)：意味着变量列表中的变量类型决定输入数据需要的格式。对于表控输入，输入数据值的类型和顺序必须与提供的输入数据的类型和顺序匹配。

### 1.7.2 输出语句(WRITE)

```fortran
WRITE(*.*)output_list
```

$\boldsymbol{output\_list}$：输出的数据项列表（变量、常数或表达式）。如果在列表中有多个数据项，那么数据项应该用逗号隔开。

$\boldsymbol{(＊,＊)}$：含有输出的控制信息。与输入语句类似。

## 1.8 变量初始化

在Fortran程序中有三种有效技术初始化变量：**赋值语句**、**READ语句**和**类型声明语句**中的**初始化**。

### 1.8.1 赋值语句初始化

格式：

```fortran
PROGRAM init_1
INTEGER:: i
i = 1
WRITE(*,*)i
END PROGRAM init_1
```

### 1.8.2 READ语句初始化

格式：

```fortran
PROGRAM init_2
INTEGER:: i
READ(*,*)i
WRITE(*,*)i
END PROGRAM init_2
```

### 1.8.3 类型声明语句初始化

格式：

```fortran
PROGRAM init_3
INTEGER::i=1
WRITE(*,*)i
END PROGRAM init_3
```

## 1.9 IMPLICIT NONE 语句

1.4中介绍到，Fortran的默认变量声明格式为：

任何以字母$i,j,k,l,m,n$开头的变量名假定为**INETEGER**，其他字母开头的变量名则假定为**REAL**。默认情况下没有变量的类型为字符型。

**IMPLICIT NONE**语句使Fortran中默认提供输入值的功能丧失。当程序含有IMPLICIT NONE语句，没有出现在显式类型声明语句中的变量被认为是错的。IMPLICIT NONE语句出现在**PROGRAM语句之后和类型声明语句之前**。

当程序含有IMPLICIT NONE语句，程序员必须显式声明程序中每个变量的类型。

在程序中始终显式地定义每个变量，用IMPLICIT NONE语句帮助在执行程序前查找和改正印刷错。

## 1.10 调试Fortran程序

为减少调试错，保证设计程序时：

1. 使用IMPLICIT NONE语句。
2. 返回所有输入值。
3. 初始化所有变量。
4. 用圆括号使赋值语句的功能更清晰。

## 1.11 小结

### 1.11.1 遵循原则

1. 尽可能给变量取**有意义的名字**。以便一瞥就可以理解变量的作用。
2. 在程序中始终用**IMPLICIT NONE**语句，以便编译时，编译器捕获打字错。
3. 在编写的程序中**创建数据字典**。数据字典应该明确地声明和定义程序的每个变量。如果是应用题，还要记得保证每个物理量要有相应的计量单位。
4. 常数的取值要始终一致。
5. 保证给所有常数指定所用机器支持的相应精度。
6. 真实世界连续变化的量不该用整型数据来计算，如距离、时间等。仅对固定值使用整型数，如计数器。
7. 除指数运算外，**尽量不要使用混合模式运算**。
8. 必要的时候用更多的**圆括号**来改进表达式的可读性。
9. 总是返回用键盘为程序提供的输入数据，以保证它们被正确地键入和处理。
10. 在使用之前，**初始化程序中的所有变量**。可以用赋值语句、READ语句和声明语句中的直接赋值来初始化变量。
11. 总是打印输出数据值相应的计量单位，计量单位对于理解程序的结果很有用。

### 1.11.2 语法小结

1. **PROGRAM语句**

   ```fortran
   !格式
   PROGRAM program_name
   !例子
   PROGRAM my_ program
   ```

   PROGRAM语句指定Fortran程序的名字，它必须是程序的第一条语句，名字必须是唯一的，不能与程序中的变量名相同。程序名由1~31个字母、数字和下划线字符组成，但是程序名的第一个字符必须是字母。

2. **END PROGRAM语句**

   ```fortran
   END PROGRAM [name]
   ```

   END PROGRAM语句必须是Fortran程序段的最后一条语句。它告诉编译器不再有语句需要处理。当遇到END PROGRAM语句，程序执行停止。END PROGRAM中的程序名是可选项。

3. **赋值语句**

   ```fortran
   !格式
   variable=expression
   !例子
   pi = 3.141593
   distance = 0.5 * acceleration * time ** 2
   side = hypot * cos(theta)
   ```

   赋值语句的左边必须是变量名。右边可以是常数、变量、函数或表达式。等号右边的数量值存储到等号左边的变量名中。

4. **STOP语句**

      ```fortran
   STOP
   STOP n
   STOP 'message'
   ```

   STOP语句停止Fortran程序的执行。一个Fortran程序可以有多条STOP语句，每条STOP语句紧接在END PROGRAM语句之前就可以省略，因为当执行到END PROGRAM语句时程序也停止。

5. **ERROR STOP语句**

   ```fortran
   ERROR STOP
   ERROR STOP n
   ERROR STOP 'message'
   ```

   ERROR STOP语句停止Fortran程序的执行，告知操作系统发生了一个执行错。

6. **IMPLICIT NONE语句**

   ```fortran
   IMPLICIT NONE
   ```

   IMPLICIT NONE语句关闭Fortran的默认类型定义。当在程序中使用这条语句，程序中的每个变量都必须在类型声明语句中显式声明。

7. **INTEGER语句**

	```fortran
	!格式
	INTEGER::variable—name1 [, variable_name2,...]
	!例子
	INTEGER::i,j,count
	INTEGER: : day=4
	```

	INTEGER语句是类型声明语句，它声明整型数据类型的变量。这条语句重载Fortran中指定的默认类型。

8. **REAL语句**

	```fortran
	!格式
	REAL: :variable_name1［ ,variable_name2,...]
	REAL::variable name=value
	!例子
	REAL::distance,time
	REAL::distance,time
	```

	REAL语句是类型声明语句，它声明实型数据类型的变量。这条语句重载Fortran中指定的默认类型。

9. **CHARACTER语句**

	```fortran
	!格式
	CHARACTER(len=<len>):: variable_name1 [,variable_name2,... ]
	CHARACTER(<len>) : : variable_name1 [,variable_name2,... ]
	CHARACTER:: variable—name1 [,variable_name2,... ]
	!例子
	CHARACTER(len=10)::first,last,middle
	CHARACTER(10)::first = 'My Name'
	CHARACTER::middle_initial
	```

	CHARACTER语句是类型声明语句，它声明字符数据类型的变量。每个变量的字符长度用(len=<len>）或$<len>$指定。如果缺省长度声明，那么变量的默认长度是1。CHA肚CTER变量的取值可以在声明的时候用字符串初始化。

10. **READ语句（表控READ)**

	```fortran
	!格式
	READ(*.*) variable_name1 [, variable_name2,... ]
	!例子
	READ(*.*)stress
	READ(*.*)distance,time
	```

	表控READ语句从标准输入设备读入一个或多个数据，并把它们加载到列表的变量名中，数据值是按列出的变量名顺序存入到变量。数据值必须由空格或逗号隔开。按多行读入数据也可以，但是**每条READ语句从一个新行开始读取数据**。

11. **WRITE语句（表控WRITE)**

	```fortran
	!格式
	WRITE(*.*)expressionl [,expression2,... ]
	!例子
	WRITE(*.*)stress
	WRITE(*.*)distance,time
	WRITE(*.*)'SIN(theta)=',SIN(theta)
	```

	表控WRITE语句把一个或多个表达式的计算值输出到标准输出设备。输出的数据值按列表中的表达顺序排列。



