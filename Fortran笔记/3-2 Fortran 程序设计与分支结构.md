# 2 程序设计与分支结构

## 2.1 逻辑常数、变量和运算符

### 2.1.1 逻辑常数和变量

逻辑数据类型：**TRUE**、**FALUE**。

逻辑常数：**.TRUE.**、**.FALUE.**，数值的两边需要有句点以区别于变量名。

逻辑变量：保存逻辑数据类型数值的变量。

声明方式：

```fortran
LOGICAL::var1[,var2,var3,...]
```

### 2.1.2 赋值语句与逻辑计算

与算术计算相似，逻辑计算是用赋值语句来完成的，其形式为：

```fortran
logical_variable_name = logical_expression
```

等号右边的表达式可以是任何有效的**逻辑常量**、**逻辑变量**和**逻辑运算符**的组合。逻辑运算符是数字、字符或逻辑数据的运算符，产生逻辑结果。逻辑运算符有两种基本类型：**关系运算符**和**组合运算符**。

### 2.1.3 关系运算符

| 新形式 | 旧形式 | 意义       |
| ------ | ------ | ---------- |
| ==     | .EQ.   | 等于       |
| /=     | .NE.   | 不等于     |
| >      | .GT.   | 大于       |
| >=     | .GE.   | 大于或等于 |
| <      | .LT.   | 小于       |
| <=     | .LE.   | 小于或等于 |

运行示例：

| 运算    | 结果    |
| ------- | ------- |
| 3<4     | .TRUE.  |
| 3<=4    | .TRUE.  |
| 3==4    | .FALSE. |
| 3>4     | .FALSE. |
| 4<=4    | .TRUE.  |
| 'A'<'B' | .TRUE.  |

关系运算符在所有的算术运算符计算之后才计算。

### 2.1.4 组合逻辑运算符

| 运算符   | 功能       | 定义                                         |
| -------- | ---------- | -------------------------------------------- |
| a.AND.b  | 逻辑与     | 如果a和b为真，则结果真                       |
| a.OR.b   | 逻辑或     | 如果a和b任一为真，则结果为真                 |
| a.EQV.b  | 逻辑等值   | 如果a和b相同（同真或同假），则结果为真       |
| a.NEQV.b | 逻辑非等值 | 如果a和b其中一个为真，另一个为假，则结果为真 |
| .NOT.a   | 逻辑非     | 如果a为假，则结果为真，如果a为真，则结果为假 |

在操作级别中，组合逻辑运算符是在所有的算术运算和所有的关系运算计算完之后才进行计算。表达式中运算符的计算顺序如下：

1. 所有的算术运算符按照以前描述的顺序先计算。
2. 所有的关系运算符(==、/=、>、＞＝、<、＜=)从左至右计算。
3. 所有的.NOT运算符进行计算。
4.  所有的.AND.运算符从左至右计算。
5.  所有的.OR.运算符从左至右计算。
6. 所有的.EQV.和.NEQV.运算符从左至右计算。

与算术运算符一样，圆括号可以用来改变默认的计算顺序。

## 2.2 控制结构：分支

分支是允许跳过其他代码段而选择执行特定代码段(称为程序块)的Fortran语句。

主要有IF语句和SELECT CASE结构。

### 2.2.1 IF结构块

IF结构块的形式：

```fortran
IF(logical_expr)THEN
Statement 1
Statement 2
....
END IF
```

注意：

- IF(…)THEN是单个Fortran语句，必须一起写在同一行上，并且要执行的语句必须占用IF (…)THEN语句下面的单独的一行。

- 紧跟其后的END IF语句必须另起一行。在包含END IF语句的行上不能有行号。

### 2.2.2 ELSE和ELSE IF子句

结构形式：

```fortran
IF(logical_expr1)THEN
Statement 1
Statement 2
....
ELSE IF(logical_expr2)THEN
Statement 1
Statement 2
....
ELSE
Statement 1
Statement 2
....
END IF
```

### 2.2.3命名的IF结构块

可以给IF结构块一个指定名称。形式为：

```fortran
[name:] IF(logical_expr1)THEN
Statement 1
Statement 2
....
ELSE IF(logical_expr2)THEN [name]
Statement 1
Statement 2
....
ELSE [name]
Statement 1
Statement 2
....
END IF [name]
```

注意：

- 名称必须唯一，并且不能与程序单元内部的任何常数和变量的名称一样。
- 如果给IF指定一个名称，那么在关联的END IF上也必须出现同样的名称
- 对于结构的ELSE和ELSE IF语句，名称是任选的，但如果使用了名称，他们必须与IF上的名称相同

### 2.2.4 逻辑IF语句

上述IF结构块还有另一种可供使用的形式。它只是单条语句，形式是：

```fortran
IF(logical_expr)statement
```

逻辑IF语句这种形式与在IF块中只带有一个语句的IF结构块等价。

### 2.2.5 SELECT CASE结构

$\bold{SELECT \,\,\, CASE}$结构是分支结构的另一种形式。一般形式：

```fortran
[name:] SELECT CASE(case_expr)
CASE(situation_1)[name]
statement 1
statement 2
....
CASE(situation_2)[name]
statement 1
statement 2
....
CASE DEFAULT [name]
statement 1
statement 2
....
END SELECT [name]
```

注意：

- 如果需要，可以给CASE结构命名。在每个程序单元中，名字必须是唯一的。
- case_expr可以是任意的整数、字符或者逻辑表达式。
- 在CASE结构中总是包含一个CASE DEFAULT子句来捕捉可能在程序中发生的任何逻辑错误或非法输入。

由于四舍五入误差会引起应该相等的两个变量在测试相等性时失败的问题，所以取代的方法是，应注意在IF结构中将测试实数变量的相等性问题改为测试**近似相等性**。

## 2.3 小结

#### 2.3.1 遵循原则

1. 总是将IF 和CASE结构块中的代码块缩进，以增加其可读性。
2. 由于四舍五入误差可能引起两个应该相等的变量在测试相等性时失败，因此注意IF结构中实数变量的相等性测试问题。取代的方法是，在所使用的计算机上测试变量在四舍五入误差范围内是否与期望的值近似相等。
3.  在CASE结构中要一直包括有DEFAULT CASE 子句，捕捉程序中可能发生的任何逻辑错误或非法输入。

#### 2.3.2 语法小结

1. IF结构块

   ```fortran
   [name:]IF(logical_expr1)THEN
   			block_1
   		ELSE IF(logical_expr2)THEN [name]
   			block_2
   		ELSE [name]
   			block_3
   		END IF [name]
   ```

   - IF结构块允许根据一个或多个逻辑表达式执行一个代码块。如果逻辑表达式1为真，就执行第一个代码块。如果逻辑表达式1为假而逻辑表达式2为真，就执行第二个代码块。如果两个逻纠表达式均为假，就执行第三个代码块。

   - 在**任一代码块被执行后**，控制程序跳到结构后面的第一条语句处。
   - 在IF结构块中必须有且只有一个IF(...)THEN语句。可以有任意多个数目的ELSE IF子句(没有或多个），并且结构中最多可有一个ELSE子句。**名字**是任意的，但是**如果用在了IF语句上，那么它也必须用在END IF语句上**。ELSE IF和ELSE语句上的名字是任意的，即使其用在了IF和END IF语句上。

2. **CASE 结构**

   ```fortran
   [name:] SELECT CASE(case_expr)
   CASE(case_1) [name]
   block_1
   CASE(case_1) [name]
   block_2
   CASE DEFAULT [name]
   block_n
   END SELECT [ name ]
   ```

   - CASE结构根据case_expr的值执行一个特定的语句块，case_expr的值可以是一个**整数**、**字符**或**逻辑值**。每个情况选择子为情况表达式指定一个或多个可能的数值。如果case_expr为包含在某一给定的情况选择子中的数值，那么就执行相应的语句块，控制程序跳到该结构结尾后面的第一条可执行语句处。

   - 如果没有可执行的情况选择子，如果存在CASE DEFAULT块就执行该语句块，控制程序跳到该结构结尾后面的第一条可执行语句处。如果CASE DEFAULT不存在，该结构就不做任何事情。
   - 在CASE结构中必须有一个SELECT CASE语句和一个END SELECT语句。可以有一个或多个CASE语句，最多可包含一个CASE DEFAULT语句。注意所有的情况选择子必须是相互独立的。名字是任意的，但是如果用在了SELECT CASE语句上，那么它也必须用在END SELECT语句上。CASE语句上的名字是任意的，即使其用在了SELECT CASE和END SELECT语句上。

3. **LOGICAL 语句**

   ```fortran
   !格式
   LOGICAL: :variable_name1 [,variable_name2,...］
   !例子：
   LOGICAL: :initialize,debug
   LOGICAL::debug = .false.
   ```

   LOGICAL语句是一种类型声明语句，声明逻辑数据类型的变量。LOGICAL变量的数值可以在声明时初始化。

4. **逻辑IF语句**

   ```fortran
   IF(logical_expr1)statement
   ```

   - 逻辑IF语句是IF结构块的一种特殊情况。如果逻辑表达式为真， 则与IF在同一行的语句被执行。然后接着执行IF语句的下一行。
   - 如果作为逻辑条件的结果只需要执行一条语句，那么该语句可用来代替IF结构块。

