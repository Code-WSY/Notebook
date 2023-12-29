# 7 数组的高级特性

## 7.1 二维数组

### 7.1.1 声明二维数组



 ```fortran
  REAL,DIMENSION(3,6)::sum
  !声明3行6列构成的实数数组，总共18个元素。
 ```

 ```fortran
  INTEGER,DIMENSION(0:100,0:20)::hist
  !声明101行21列的整数数组。第一个下标0-100，第二个下标0-20。
 ```

 ```fortran
  CHARACTER(len=6),DIMENSION(-3:3,10)::counts
  !声明7行10列的数组，数组的类型是字符型，每个数组元素包含6个字符
 ```

 ```fortran
  INTEGER::x(10,10)
 !声明10行10列的整数数组。
 ```

  

### 7.1.2 二维数组的储存

计算机内存中的排列：Fortran 语言总是以**列**为主顺序为数组元素分配空间。

$a(1,1),a(2,1),a(3,1),a(1,2),a(2,2),a(3,2),...$

### 7.1.3初始化二维数组

**初始化方法：**

1. **赋值语句**

   **DO循环**

   ```fortran
   INTEGER,DIMENSION(4,3)::ISTAT
   DO i=1.4
   	DO j=1,3
   	ISTAT(i,j)=j		
   	END DO
   END DO
   ```

   **RESHAPE函数**

   一个特殊的内部函数，它可以在不改变数组中元素的个数情况下，改变一个数组的结构：

   ```fortran
   !格式：output=RESHAPE(array1,array2)
   !array1:数据
   !array2:描述新结构的一维数组
   istat=RESHAPE([1,1,1,1,2,2,2,2,3,3,3,3],[4,3])
   ```

   RESHAPE函数把一个1$\times$12 的数组转化为一个4$\times$3的数组， 然后赋给istat，数值顺序参考7.1.2

2. **类型声明语句**

   ```fortran
   INTEGER,DIMENSION(4,3)::istat(4,3)=&
   RESHAPE([1,1,1,1,2,2,2,2,3,3,3,3,],[4,3])
   ```

   用于初始化数组的值必须和数组具有同样的结构，所以必须使用RESHAPE函数。

3. **READ语句**

   如果在一条READ语句的参数列表中出现了一个没有下标的数组名，那么程序将会为数组中的所有元素读取数值， 这些数值将会按照数组元素在计算机内存中的逻辑顺序为数组赋值。

   如果文件INITIAL.DAT包含：

   ```fortran
   1 1 1 1 2 2 2 2 3 3 3 3
   ```

   可通过下面代码实现生成4$\times$3数组：

   ```fortran
   INTEGER,DIMENSION(4,3)::istat
   OPEN(UNIT=7,FILE='INITIAL.DAT',STATUS='OLD',ACTION='READ')
   READ(7,*) istat
   ```

   也可利用隐含的DO循环：
   
   ```FORTRAN
   INTEGER,DIMENSION(4,3)::istat
   INTEGER::i,j
   OPEN(UNIT=7,FILE='INITIAL.DAT',STATUS='OLD',ACTION='READ')
   READ(7,*) ((istat(i,j),i=1,4),j=1,3)
   ```
   

## 7.2 多维数组

Fortran语言支持下标多达**15**个的复杂数组。这些大数组的声明、初始化和使用方式都和前面章节中介绍过的二维数组相同。

注意：

当第一个下标的取值范围取值完后，第二个下标才增加1，第二个下标取值范围取值完后，第三个下标才增1。所有数组定义的下标都重复地按这个过程使用，第一个下标总是改变的最快，最后一个下标总是改变的最慢。如果需要对n维数组进行初始化或进行I/O操作，那么就必须牢记这种分配结构。

## 7.3 对数组使用Fortran内置函数

### 7.3.1基本内置函数

基本内置函数是使用标量参数的函数， 它也可以适用于数组参数。

如果一个基本函数的参数是一个标量， 那么这个函数的返回值也应该是一个标量。

如果函数的参数是一个数组，那么函数的返回值应该也是一个和输入数组相同结构的数组。

部分基本内置函数：ABS、SIN、COS、TAN、EXP、LOG、LOGIO、MOD 以及SQRT等

### 7.3.2 查询内置函数

**部分常用的数组查询函数**

| 函数名称和调用序列 | 用途                                                         |
| ------------------ | ------------------------------------------------------------ |
| ALLOCATED(ARRAY)   | 判断可分配数组的分配状态                                     |
| LBOUND(ARRAY,DIM)  | 如果缺少DIM, 返回所有的ARRAY 下界：如果给出了DIM, 返回指定的ARRAY下界。如果DIM 缺省，结果是一个一维数组，如果给出了DIM, 结果是一个标量 |
| SHAPE(SOURCE)      | 返回数组SOURCE的结构                                         |
| SIZE(ARRAY,DIM)    | 如果给出了DIM返回指定维度的ARRAY的宽度， 否则返回数组中元素的总个数 |
| UBOUND(ARRAY,DIM)  | 如果缺少DIM, 返回所有的ARRAY上界；如果给出了DIM, 返回指定的ARRAY上界。如果DIM缺省，结果是一个一维数组，如果给出了DIM, 结果是标量 |

### 7.3.3 变换内置函数

|                                  |                                                              |
| -------------------------------- | ------------------------------------------------------------ |
| 函数名称                         | 用途                                                         |
| ALL (MASK)                       | 如果数组MASK中的所有元素值都为真，逻辑函数返回TRUE           |
| ANY (MASK)                       | 如果数组MASK中的任意元素值为真，逻辑函数返回TRUE             |
| COUNT (MASK)                     | 返回数组MASK中为真元素的个数                                 |
| DOT_PRODUCT(VECTOR_A , VECTOR_B) | 计算两个大小相等的向址的点积                                 |
| MATMUL (MATRIX_A , MATRIX_B)     | 对两个一致的矩阵执行矩阵乘法                                 |
| MAXLOC (ARRAY, MASK)             | 返回MASK为真对应的ARRAY中的元素的最大值的位置，结果是带有一个元素的一维数组，这个数组元素是ARRAY中的下标值(MASK是可选的） |
| MAXVAL (ARRAY, MASK)*            | 返回MASK为真对应的ARRAY中的元素的最大值(MASK是可选的）       |
| MINLOC (ARRAY, MASK)             | 返回符合MASK为具的ARRAY中的元素的最小值的位置，结果是带有一个元素的一维数组， 这个数组元素是ARRAY中的下标值(MASK是可选的） |
| MINAL (ARRAY,MASK)*              | 返回符合MASK为真的ARRAY中的元素的最小值(MASK是可选的）       |
| PRODUCT (ARRAY,MASK)*            | 计算ARRAY中MASK为其的元素的乘积。MASK为可选的：如果不提供， 计算数组中所有元素的乘积 |
| RESHAPE (SOURCE, SHAPE )         | 构造一个数组，它的结构由数组SOURCE 中的元素指定。SHAPE是一个一维数组，它包含了将要建造的数组的每个维度的宽度值 |
| SUM (ARRAY, MASK)*               | 计绊ARRAY中MASK为具的元素的和。MASK为可选的；如果不提供，计算数组中所有元素的和 |
| TRANSPOSE (MATRIX)               | 返回一个倒置的二维矩阵                                       |

*如果函数中使用MASK,必须定义为MASK=mask_expr, 其中，mask_expr是指定**掩码**的逻辑数组。

## 7.4 加掩码的数组赋值：WHERE结构

在Fortran中，使用WHERE结构或语句可实现掩码数组赋值。

WHERE结构：

```fortran
[name:]WHERE(mask_expr1)
Array Assignment Statement(s) !块1 数组赋值语句
ELSEWHERE(mask_expr2)[name] 
Array Assignment Statement(s) !块2
ELSEWHERE(mask_expr3)[name]
Array Assignment Statement(s) !块3
END WHERE[name]
```

举例：对数组(value)中正数求对数值，其余取-99999

```fortran
WHERE(value>0.)
	logval=LOG(value)
ELSEWHERE
	logval=-99999.
END WHERE
```

表达式'value>0. '生成一个逻辑数组，当value 数组中的对应元素大于0时，这个数组中的元素为TRUE, 当value 中的元素小于或等于0时，这个数组中的元素为FALSE。所有这个**逻辑数组**可以作为一个**掩码条件**来控**制数组赋值语句的操作**。

**WHERE 结构更加优于逐个元素完成运算，尤其是对于多维数组。**

## 7.5 FORALL结构

Fortran 也包含了一个结构， 该设计结构允许一系列操作用于数组中部分元素，且是逐个用到数组元素上的。被操作的数组元素可以通过**下标索引**和通过**逻辑条件**来进行选择。只有那些索引满足约束和逻辑条件的数组元素才会被操作。这种结构称为FORALL结构。

```fortran
[name:]FORALL(in1=triplet1[,in2=triplet2,...,logical_expr1])
Statement 1
Statement 2
...
Statement n
END FORALL [name]

```

FORALL语句中的每个索引都是通过下标的三元组形式来指定的：

```fortran
subscript_1: subscript_2: strid
```

subscript_1:索引的开始值

subscript_2:索引的结束值

strid:增量值

举例1：10$\times$10的特征矩阵，对角线为1，其余位置为0.

```fortran
REAL,DIMENSION(10,10):i_matrix=0.
FORALL(i=1,10)
	i_matrix(i,i)=1.0
END FORALL
```

举例2：

```fortran
FORALL{i=l:n,j=l:m,work(i,j)/=0.)
work(i,j)=l./work(i,j)
END FORALL
```

若数组各个元素非0可直接使用简单语句:

```fortran
work = 1./work
```

一般说来，任何一个可以用FORALL结构表示的表达式也可以用包含有IF结构的嵌套DO结构来表示。

不同之处：DO循环结构中的语句必须按照一种严格的顺序来执行，而FORALL 结构中的语句可以按照任意次序来执行。这种自由意味着使用大型并行计算器**可以优化程序**，通过给每台独立的处理器分配元素来最大地**提高运行速度**。处理机可以以任何次序来完成它们的工作，而不会对最终的结果产生影响。

如果FORALL 结构体中包含了不止一条语句，那么处理器首先完成第一条语句所涉及的**所有元素的处理后**， **才开始第二条语句元素的操作**。

Fortran 也包含一条单行FORALL 语句：

```fortran
FORALL(indl=tripletl [,..., logical_expr]) Assignment Statement
```

##  7.6 可分配数组

使用动态内存分配，**在每次执行的时候动态地设置数组大小使得它的大小足够解决当前问题。**

### 7.6.1 ALLOCATABLE属性

在类型声明语句中使**用ALLOCATABLE属性来声明动态分配内存的数组**，它使用ALLOCATE语句实际分配内存。当程序使用完内存之后，应该使用EDALLOCATE语句释放内存，以供其他用户使用。

结构：

```fortran
REAL,ALLOCATABLE,DIMENSION(:,:)::arrl
```

注意： 因为不知道数组实际的大小，所以在声明中使用冒号作为占位符。在类型声明语句中定义的是数组的维数而不是数组的大小。

数组的实际大小通过ALLOCATE语句来指定。

- 格式1：

```fortran
!ALLOCATE（list of arrays,STAT=status,ERRMSG=err_msg)
!ALLOCATE(array to allocate,SOURCE=source_expr,STAT=status,ERRMSG=string)
!例：
ALLOCATE(arr1(100,0:10),STAT=status,ERRMSG=msg)
```

这条语句在执行时分配数组arr1为一个100$\times$11的数组。STAT和ERRMSG子句是可选的。如果出现这一句，将返回一个整数状态值。分配成功状态为0, ERRMSG的值将不再变化。如果分配失败， 则STAT＝返回一个非0值，用来指示错误类型，ERRMSG＝的值包含描述信息，用来告诉用户问题所在。

- 格式2：

```fortran
ALLOCATE(myarray,SOURCE=source_array,STAT=istat,ERRMSG=msg)
```

分配数据与源表达式结构相同，源表达式的数据将被复制到新分配的数组中。

在任何一个ALLOCATE语句中， 始终应该包含STAT子句，来检查返回值的状态，如果在给数组分配必要的空间时出现内存不足的情况，程序可以友好地终止。

没有给数组分配内存空间之前， 在程序中不可以以任何方式使用可分配数组。

引用可分配数组前可测试其状态(ALLOCATED函数)：

```fortran
REAL,ALLOCATABLE,DIMENSION(:) ::input_data
IF(ALLOCATED(input_data))THEN
READ (8,*)input_data
ELSE
WRITE(*,*)'Warning:Array not allocated!'
END IF
```

使用分配数组最后应该释放内存：

```fortran
DEALLOCATE(list of arrays to deallocate,STAT=status)
```

例：

```fortran
DEALLOCATE(arrl,STAT=status)
```

### 7.6.2 在赋值语句中使用Fortran可分配数组

我们已经学习了如何使用ALLOCATE和DEALLOCATE语句来分配和释放可分配数组。此外，Fortran 2003和更高版本允许通过简单地赋值数据来自动分配和释放可分配数组。

假如要将一个表达式赋给一个同样维数的可分配数组， 那么如果该数组没有分配， 将自动为数组分配正确的结构， 或者如果先前分配的结构和要求不一致， 系统将会自动地释放空间， 并重新分配给它一个正确的结构。

**不需要ALLOCATE 和DEALLOCATE 语句。**

如果所赋的数据结构和已分配的结构相同，则该数据不需重新分配便可重新使用。

这就意味着**在运算中数组可以无缝使用**。

```fortran
PROGRAM test_allocatable—arrays
IMPLICIT NONE
!声明数据
REAL,DIMENSION(:),ALLOCATABLE:: arrl
REAL,DIMENSION(8)::arr2 = [ 1.,2.,3.,4.,5.,6.,7.,8.]
REAL,DIMENSION(3):: arr3 = [ 1., -2., 3. ]
!自动把数组arrl按3个元素值的规模来分配
arrl=2.*arr3
WRITE(*,*)arrl
!自动把数组arrl按4个元素值的规模来分配
arrl=arr2(1:8:2)
WRITE (*,*)arrl
!按4个元素值的规模复用数组arrl，不回收空间
arrl=2.*arr2(1:4)
WRITE (*,*)arrl
END PROGRAM test_allocatable_arrays
```

在<u>子例程</u>或者<u>函数</u>中没有声明**SAVE属性**的**可分配函数**， 当子例程或函数退出时会自动释放。不再需要DEALLOCATE语句。

### 7.6.3 再谈SAVE

函数子程序或子例行程序中用到的所有**变量**，在被调用前通常都没有确定的存储单元，每当子程序被调用时才临时分配存储单元，而且在退出子程序时这些存储单元又都被释放并重新分配另作它用。所以这些变量的值不被保留。

在下次进入子程序时，给这些变量分配的可能是另外一些存储单元，上次调用时这些变量所具有的值已不复存在。我们称这些变量在子程序未调用时是**无定义**的。

在函数或子例行程序中可以使用SAVE说明语句来指定子程序中的某些变量的存储单元**不被释放**，它们的内容在退出子程序期间保持不变，在下次调用时仍可使用。

SAVE语句的形式如下：

```fortran
SAVE
SAVE N,M
```

## 7.7 小结

### 7.7.1 遵循原则

1. 使用RESHAPE函数改变数组的结构。当利用数组构造器创建所需结构的数组常量时，这一函数非常有用。

2. 使用隐含的DO循环来读写二维数组，数组的一行看作是输入或输出文件的一行。这种方式可以使程序员更容易将文件中的数据和程序中表示的数据建立联系。
3. 如果只是希望修改和赋值那些通过某些测试的元素，可以使用WHERE结构来修改和赋值数组元素。
4. 使用可分配数组来产生程序， 可以动态的调节内存需求以适应于所解决问题的大小。用ALLOCATABLE属性声明可分配数组， 用ALLOCATE语句分配内存， 用DEALLOCATE语句释放内存。
5. 在任何ALLOCATE语句中始终应该总是包含STAT，始终检查返回的状态，以便如果没有足够的空间分配给数组，可以使程序友好地退出。
6. 一旦使用完可分配数组之后，始终应该用DEALLOCATE语句释放可分配数组。
7. 当在Fortran 2003或后续程序中使用可分配数组时，为保证数据和可分配数组有相同的维数，将自动调整数组大小，使其与已分配的数据大小一致。

### 7.7.2 语法小结

1. ALLOCATABLE属性：

   ```fortran
    !格式:
    type,ALLOCATABLE,DIMENSION(:, [:,... ]) :: ARRAYl,...
    !示例：
    REAL,ALLOCATABLE,DIMENSION(:) ::arrayl
    INTEGER,ALLOCATABLE,DIMENSION(:, :, :) : :indices
    !说明：
    ! ALLOCATABLE属性声明数组的大小是动态的。
    ! 大小在运行时由ALLOCATE语句指定。
    ! 类型声明语旬必须指明数组的维数，但不用说明每个维度的宽度。每个维度用冒号来指定。
   ```
   
2. ALLOCATABLE语句：

   

   ```fortran
    !格式：
    ALLOCATABLE:: arrayl,...
    !示例：
    ALLOCATABLE: :arrayl
    !说明：
    !ALLOCATABLE 语句声明数组的大小是动态的，当关联类型声明语句，它实现ALLOCATABLE属性的功能。
    !不要再使用这个语句了，而应该使用ALLOCATABLE属性。
   ```

3. ALLOCATE语句：

   

   ```fortran
   !格式：
   ALLOCATE(arrayl([il:]i2,[ jl:]j2,...),...,STAT=status)
   ALLOCATE(arrayl,SOURCE=expr,STAT=status,ERRMSG=msg)
   !示例：
   ALLOCATE(arrayl(lOOOO),STAT=istat)
   ALLOCATE(indices(-10:10,-10:10,5),STAT=istat)
   ALLOCATE(arrayl,SOURCE=array2,STAT=istat，ERRMSG=msg)
   !说明：
   !ALLOCATE语句为前面声明的数组动态分配内存空间。在ALLOCATE语句的第一种形式中，ALLOCATE语句指定每个维度的宽度。如果成功完成，返回状态为O, 一旦出错，返回状态将会是一个依赖于机器的正数。
   !在ALLOCATE 语句的第二种形式中，数组大小和原数组大小一致，数组的维度的宽度也和原数组一致。
   !在ALLOCATE 语句的第二种形式中，SOURCE和ERRMSG仅在Fortran2003和后续版本才支持。
   ```

4. DEALLOCATE语句：

   

   ```fortran
   !格式：
   DEALLOCATE (arrayl,..., STAT=status, ERRMSG=msg)
   !示例：
   DEALLOCATE(arrayl,indices,STAT=status)
   !说明：
   !DEALLOCATE语句动态的释放ALLOCATE语句为一个或多个可分配数组分配的内存。当该语句执行完成之后，关联那些数组的内存将不再能够被访问。成功执行返回状态为O, 一旦出错，返回状态将会是一个依赖于机器的正数。
   ```

   

5. FORALL结构：

   

   ```fortran
   !格式：
   [name:] FORALL (indexl=triplet1 [,... logical_expr])
   Assignment Statement(s)
   END FORALL [name]
   !示例：
   FORALL(i=l:3,j=l:3,i>j)
   arrl(i,j)=ABS(i-j)+3
   END FORALL
   !说明：
   !FORALL结构用y为那些用三元组下标和可选的逻辑表达式标识下标的数组元素执行赋值语句， 但是并不能够指定它们执行的次序。可能有许多数组元素的下标符合要求， 且每个索引值用下标三元组指定。逻辑表达式当成掩码作用于数组的下标索引， 使得操作的仅是那些逻辑表达式为TRUE的特定下标索引组指定的数组元素。
   ```

   

6. FORALL语句：

   

   ```fortran
   !格式：
   FORALL(indexl=tripletl [,...,logical—expr])Assignment Statement
   !说明:
   !FORALL语句是FORALL结构的简化版本，这条语句中只有一个赋值语句。
   ```

7. WHERE结构：

   

   ```fortran
   !格式
   [name:] WHERE (mask_exprl)
   Module 1
   ELSEWHERE (mask—expr2) [name]
   Module 2
   ELSEWHERE[name]
   Module 3
   END WHERE [name]
   !示例：
   WHERE(input>1000.)
   input=1000.
   ELESWHERE(input<-1000.)
   input=-1000.
   END WHERE
   !说明:
   !WHERE结构允许操作用于与给定条件匹配的数组元素，另一些不同操作集用于不匹配的元素。每个mask_exprl必须是与用代码块操作的数组一样结构的逻辑数组。
   !在这个结构中，ELSEWHERE子句是可选的。可以有想要的很多有掩码的ELSEWHERE子句，直到遇到一个简单的ELSEWHERE为止。
   ```

   

8. WHERE语句：

   

   ```fortran
   !格式
   WHERE(mask expression)array_assignment statement
   !说明：
   !WHERE语句是WHERE 结构的简化版本，在这个语句中只有一条数组赋值语句，没有ELSEWHERE子句。
   ```

   

