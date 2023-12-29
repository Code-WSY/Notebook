# 12 过程和模块的高级特性

## 12.1 作用范围和作用域

Fortran 程序有四种**作用范围**：

1. **全局范围**

   全局对象是指能作用于**整个程序**而定义的对象。

   在一个程序中，这些对象的名字必须不同。到目前为止，我们所遇到的全局变量就是**程序名**、**外部过程名**以及**模块名**。在一个完整的程序中，这些名字都必须是**唯一的**。
   

2. **局部范围**

   局部对象是在某个**作用域内定义**的，并且在此**作用域内名称唯一**的对象。

   作用域的典型例子就是程序、外部过程和模块。某个作用域范围内的局部对象在该作用域内必须唯一，但是对象名、语句标号等**可以在其他作用域内再次使用**， 而不会引起冲突。

3. **块范围**

   块是程序或过程中**可以定义自己的局部变量的结构体**， 其局部变量与过程内的变量相互独立。

4. **语句范围**

   某些对象的作用范围可以限制到**程序内的单条语句**。我们看到的唯一的作用范围限制到单条语句的例子就是数组构造器中的**隐式DO变量**和**FORALL语句**中的下标变量。

## 12.2 块

块是Fortran 2008 新引入的一种结构体类型。

块是宿主程序或过程内的任意一段代码块，它以BLOCK语句开始，以END BLOCK语句结束。

块可以包含任何所需的代码，可以定义专属于块的局部变量。

格式：

```fortran
[name:] BLOCK
Type definitions ...
...
Executable code
IF (...) EXIT [name]
...
END BLOCK [name]
```

注意在块中任意位置可以使用EXIT语句退出一个代码块。

如果退出了块，代码执行从块尾的第一个执行语句开始。

每个块可以在块内的可执行代码前定义局部变量。当代码块执行结束后，块中定义的所有变量变为未定义。

如果在块内定义一个可分配的**未使用SAVE属性的数组**，当块执行结束后，这个数组将会**自动被释放**。

一个块也可以通过**宿主关联**访问其宿主的局部变量，块定义了同名局部变量的情况除外。

示例：

```fortran
PROGRAM test_block
    INTEGER::i,j,k
    i=3
    j=2
    k=1
    WRITE(*,*)'Before block: ',i,j,k
BLOCK
    INTEGER::k
    DO k=1,10
        WRITE(*,*)'In BLOCK: ',i,j,k
        IF(k>3)THEN
            EXIT
        END IF
    END DO
END BLOCK
WRITE(*,*)'After block: ',i,j,k
END PROGRAM
```

注意：这里块中定义与宿主关联相同的变量k，因此不能访问宿主变量k，得到结果：

```fortran
 Before block:            3           2           1
 In BLOCK:            3           2           1
 In BLOCK:            3           2           2
 In BLOCK:            3           2           3
 In BLOCK:            3           2           4
 After block:            3           2           1
```

若去掉第八行的块中局部变量声明，则块中可以访问修改宿主中的变量大小。

## 12.3 递归过程

一个普通的Fortran过程不可以直接或间接的调用自己。

为了解决此类问题，Fortran允许**子例程**和**函数**被声明为递归的。如果一个过程声明为递归的，那么Fortran编译器就会以直接或间接调用该过程本身的方式来实现该过程。  

### 12.3.1 子例程递归

通过将关键字**RECURSIVE**添加到SUBROUTINE语句上， 就可以将一个子例程声明为递归的。

示例：阶乘子例程

```fortran
!n>17 整数会溢出,解决方法：
!1. 不要用递归 
!2. 用大数运算库。
RECURSIVE SUBROUTINE Factorial(n,res)
    IMPLICIT NONE
    INTEGER,INTENT(IN)::n
    INTEGER(KIND=8),INTENT(OUT)::res
    INTEGER(KIND=8)::temp
    IF(n>=1)THEN
         CALL Factorial(n-1,temp)
        res=n*temp
    ELSE IF(n==0)THEN
        res=1
    ELSE
        WRITE(*,*)'Integer n must be a natural number'
    END IF
END SUBROUTINE


PROGRAM test_Factorial
    IMPLICIT NONE
    INTEGER::res
    INTEGER::N
    PRINT*,'Please enter your factorial to calculate: '
    READ(*,*)N
    CALL Factorial(N,res)
    WRITE(*,'(A,I20)')'Result: ',res
END PROGRAM
```

### 12.3.2 函数递归

函数是通过在表达式中使用函数名来调用的，而从函数返回的值是被赋值给函数名的。因此，如果函数要调用自己，当设置函数返回值时，函数名就要出现在赋值语句的左边；当递归调用函数自身时，函数名要出现在赋值语句的右边。这种函数名的双重用法确实可能会引起混乱。

为了避免递归函数中函数名的两种用法带来的麻烦， Fortran允许程序员为**递归调用函数**及其**返回结果**指定两个**不同的名字**。

**当让函数调用自己时， 使用该函数的实际名字； 当想指定一个返回值时， 使用特定的形参。**

格式：

```fortran
RECURSIVE FUNCTION factorial(n) RESULT(anwser)
!递归函数factorial：在程序中进行递归操作
!形参answer:来接收返回到调用程序的返回值，作为最终输出值：
```

如果一个函数中包含RESULT子句，那么这个**函数名就不能出现在这个函数的类型声明语句中**，**取而代之的是声明形参结果变量名。**

示例：递归函数

```fortran
RECURSIVE FUNCTION Factorial(n) RESULT(anwser)
    IMPLICIT NONE
    !类型声明，不能出现Factorial，取而代之的是anwser
    INTEGER,INTENT(IN)::n
    INTEGER::anwser

    IF(n>=1)THEN
         anwser=n*Factorial(n-1)
    ELSE IF(n==0)THEN
        anwser=1
    ELSE
        WRITE(*,*)'Integer n must be a natural number'
    END IF
END FUNCTION


PROGRAM test_Factorial
    IMPLICIT NONE
    INTEGER::Factorial
    INTEGER::N
    PRINT*,'Please enter your factorial to calculate: '
    READ(*,*)N
    WRITE(*,'(A,I20)')'Result: ',Factorial(N)
END PROGRAM
!n>17 整数会溢出,解决方法：1. 不要用递归~ 2. 用大数运算库。

```

Fortran 2015是将要构建的下一个Fortran标准。在这个标准中，所有子例程和函数默认是可递归的。如果特别想把一个过程设置成非递归的，可以使用一个新的关键字NON_ RECURSIVE声明该过程。

不要指望在不久的将来能看到这个新特性，编译器供应商需要很多年才能赶上标准的变化。

## 12.4 关键字参数和可选参数

### 12.4.1 关键字参数

如果过程接口是**显式**的，那么可以用关键字参数增加调用程序的灵活性。关键字参数是一种形式如下的参数：

```fortran
keyword = actual_argument
```

keyword是与实参关联的形参的名字。如果过程调用使用关键字参数，那么就可以任何顺序排列调用参数，因为关键字允许编译器对实参和形参进行**自动匹配**。

示例：

```fortran
MODULE procs
    CONTAINS
        REAL FUNCTION calc(first,second,third)
            IMPLICIT NONE
            REAL,INTENT(IN)::first,second,third
            calc=(first-second)/third
        END FUNCTION
END MODULE

PROGRAM test_keywords
    USE procs
    IMPLICIT NONE
    WRITE(*,*)calc(1.,2.,3.)
    WRITE(*,*)calc(first=1.,second=2.,third=3.)
    WRITE(*,*)calc(1.,third=3.,second=2.)
END PROGRAM
```

关键字参数的调用方法：

1. 常规调用

   形参与实参完全匹配， 包括个数、类型和顺序

   ```fortran
   WRITE(*,*)calc(1.,2.,3.)
   ```

2. 关键字参数调用

   如果过程接口是显式的，那么就可以改变参数列表中参数调用的顺序，增加调用程序的灵活性。

   ```fortran
   WRITE(*,*)calc(first=1.,second=2.,third=3.)
   ```

3. 混合使用常规参数和关键字参数调用

   ```fortran
   WRITE(*,*)calc(1.,third=3.,second=2.)
   ```

   第一个参数是常规参数，后面的参数都是关键字参数

   将常规调用参数和关键字参数混合使用是合法的，但是一旦一个关键字参数出现在参数表中，那么该列表中**后面的所有其他参数都必须是关键字参数**。

### 12.4.2 可选参数

可选参数是指在调用过程时并不一定出现的过程形参。过程只使用出现的形参，如果不出现，过程则不用它。可选参数仅可用于显式接口过程中。通过在形参声明中加入OPTIONAL属性，指定可选参数。

格式：

```fortran
INTEGER,INTENT(IN),OPTIONAL::upper_limit
```

包含可选参数的过程**必须有办法确定该过程执行时， 可选参数是否出现。**这个办法就是使用FORTRAN自带的逻辑函数**PRESENT**来完成，如果可选参数出现，那么此函数会返回一个真值，反之则返回假值。

格式：

```fortran
IF(PRESENT(upper_limit))THEN
...
ELSE
...
THEN
```

示例：**SELECTED_ READ_ KIND函数**

该函数接收两个参数，一个是实数的精度参数p, 另一个是实数取值范围参数r。两个参数默认的顺序是(p, r)，如果实参按照这个顺序给出，那么就不要关键字。如果实参是无序的，或者只指定了取值范围参数r, 那么就必须使用关键字。下面及时几个正确用法：

```fortran
kind_num=SELECTED_ READ_ KIND(13,100)
kind_num=SELECTED_ READ_ KIND(13)
kind_num=SELECTED_ READ_ KIND(p=13,r=100)
kind_num=SELECTED_ READ_ KIND(r=100)
```

寻找极值位置

## 12.5 过程接口和接口块

如果过程要使用诸如**关键字参数**和**可选参数**此类的Fortran高级特性，那么程序单元必须有**过程显式接口**。

另外，显式接口还可以让编译器捕获过程之间调用顺序的很多错误。没有显式接[ 抱抱]口，这些错误可能导致很小且难以发现的bug。

创建显式接口最简单的方法是**将过程放在模块中**，然后在调用程序单元使用该模块。任何放在模块中的过程均有显式接口。不幸的是，有时将过程放在模块中，并不是很方便甚至不太可能。

### 12.5.1 创建接口块

当不可能将过程放在模块中时，Fortran 允许在调用程序单元中定义一个接口块。接口块指定了外部过程所有的接口特征，编译器根据接口块中的信息执行一致性检查，应用诸如关键字参数的高级特性。

格式：

```fortran
INTERFACE
	Interface_body_1
	Interface_body_2
	...
END INTERFACE
```

每个interface_body都由以下三部分组成：

1. 相应外部过程的初始**SUBROUTINE**和**FUNCTION**语句

2. 与过程参数相关的特定**类型声明语句**(::)

3. 以及**END SUBROUTINE** 或**END FUNCTION** 语句

   这些语句为编译器给出了调用程序和外部过程之间接口一致性检查的足够信息。

当使用接口时，将它和其他类型声明语句一起放在调用程序单元的**最前面**。

**示例：**

```fortran
PROGRAM interface_test
    IMPLICIT NONE
    !----------创建sort接口-----------------！
    INTERFACE
        SUBROUTINE sort(n,array)
          IMPLICIT NONE
          REAL,ALLOCATABLE,DIMENSION(:),INTENT(INOUT),OPTIONAL::array
          INTEGER,INTENT(IN)::n
        END SUBROUTINE
    END INTERFACE
    !--------------------------------------！
    INTEGER::k
    READ(*,*)k
    !调用sort子例程
    CALL sort(k)
END PROGRAM

!原sort子例程
 SUBROUTINE sort(n,array)
            IMPLICIT NONE
            REAL,ALLOCATABLE,DIMENSION(:),INTENT(INOUT),OPTIONAL::array
            INTEGER,INTENT(IN)::n
            INTEGER::i
            REAL,ALLOCATABLE,DIMENSION(:)::arr
            !READ(*,*)n
            ALLOCATE(arr(n))
            !DO i=1,n
            !    array(i)=i
            !END DO
            arr=[(i,i=1,n)]
            WRITE(*,*)arr
END SUBROUTINE

```

### 12.5.2 接口块使用注意事项

1. 任何时候尽可能避免使用将过程放到模块中的方法来创建接口块。
2. 接口块不应该指定通过使用USE关联已经在模块中存在的过程接口。这会造成显式接口的二次定义，是非法的，会引起编译器错误。
3. 如果必须为**很多过程**创建接口，那么将所有的接口**放在一个模块中**，这样许多程序单元就能通过使用USE 关联轻松访问它们。
4. 每个接口都是一个独立的作用域，所以同样的变量名可以出现在接口和包含该接口的同一程序中而不引起混乱。
5. 接口块中形参的名字不需要和相应过程中形参名相同。
6. 接口块是独立的作用域，所以接口块中使用的形参变量必须在块中单独声明，即使这些变量在相关的作用域中已经被声明过了。

## 12.6通用过程

通用函数是指能正确操作多种不同类型的输入数据，特定函数是指一种需要特定类型输入数据的函数。

### 12.6.1 用户定义的通用过程

除了嵌入在编译器的标准函数外， Fortran 还允许我们定义我们用户自己的通用过程。

使用特定版本的通用接口块来定义通用过程。如果给INTERFACE语句加一个通用名，那么在接口块中定义的每个过程接口都可以看作一个特定版本的通用过程。

格式：

```fortran
INTERFACE generic_name
	Specific_interface_body_1
	Specific_interface_body_2
END INTERFACE
```

当编译器遇到这个含有通用接口块的程序中的通用过程名，它就会检查调用这个通用过程时关联的参数，以便确定应该使用哪个特定的过程。

为便于编译器决定该使用哪个特定的过程， 块中的每个特定过程都必须与其他特定过程能明显区分开。例如，一个特定的过程可能需要实型输入数据，而另一个需要整型输入数据等。

编译器通过比较通用过程和特定过程的调用参数表来决定使用哪个过程。

下面的规则适用于通用接口块中的特定过程：

1. 要么通用接口块中的所有过程**都是子例程， 要么都是函数**。不能将二者混在一起， 因为定义的通用过程要不就是子例程，要不就是函数，不可能同时是两者。
2. 块中的每个过程必须能通过**类型**、**个数**和**不可选参数的位置**来与其他过程区分开。只要每个过程都不同于块中的其他过程，那么编译器就能通过比较类型、个数及特定过程的调用参数的位置来决定该使用哪个过程。

## 12.7 用户自定义操作符和赋值符扩招Fortran

对于派生数据类型来说，既没有现成的一元运算符，也没有二元运算符。

可以随意使用派生数据类型中的元素，但是不能使用派生数据类型本身。这一严重的缺陷降低了派生数据类型的可用性。

Fortran允许程序员为原有的数据类型或者派生数据类型定义新的一元或二元操作符， 允许为派生数据类型定义标准操作符的新扩展功能。

定义方法：

1. 第一步是先写一个**完成目标任务的函数**(当且仅当一个函数有显式接口时，可以创建有派生数据类型的函数。)， 然后**将它放在模块中**。例如， 如果想完成两个派生数据类型变量的加法， 首先应该创建一个函数， 其参数是待加的两个变量，结果是两个变量之和。这一函数将完成执行加法指令的操作。

2. 下一步是使用接口操作符块将此函数与用户自定义或自带的操作符相关联。

   格式：

   ```fortran
   INTERFACE OPERATOR(operator_symbol)
   MODULE PROCEDURE function_1
   END INTERFACE
   ```

   其中operator_symbol是任何标准的内置操作符(+, -, x, /, >, ＜等）或用户自定义操作符。

   - 用户自定义操作符是以**点号开头和结束**的最长包含63个字符的序列（<u>数字和下划线不允许出现在操作符名中</u>）。例如，.INVERSE. 就是一个用户自定义的操作符。

   - 同一个操作符可以关联多个函数，但是这些函数必须能通过形参类型的不同而区分开。
   - 如果与同一个操作符关联的函数有两个形参，那么操作符就是一个二元操作符。如果函
     数只有一个形参，那么该操作符就是一个一元操作符。

如果由接口定义的操作符是Fortran的自带操作符(+, -,*, /, >, ＜等），那么有3个**额外的约束**需要考虑：

1. 不能修改针对预定义的自带数据类型的自带操作符的含义。例如，当加法运算符(+)用于两个整型数据时， 不能改变加法原有的动作。唯一有可能的是，当操作符用于派生数据类型，者派生数据类型和自带数据类型的组合时，可通过定义执行的动作来扩展操作符的含义。
2. 函数中参数的个数必须和该操作符的普通用法一致。例如， 乘法（＊）是一个二元操作符，所以任何扩展其含义的函数都必须有两个参数。
3. 如果扩展了关系运算符，那么不管以何种方式改写了该运算符，其扩展含义都要一致。例如，如果扩展关系运算符“ 大于", 那么这种扩展同时适用于“ > ” 或“ GT"（这两种写法都是大于运算），并且含义一致。

示例：三维矢量操作

```fortran
!当且仅当一个函数有显式接口时，可以创建有派生数据类型的函数。
MODULE vectors
IMPLICIT NONE

!------------创建派生数据类型------------!
TYPE vec
    REAL::x
    REAL::y
    REAL::z
END TYPE
!--------------------------------------!

!---将函数与自定义或自带的操作符相关联---!
INTERFACE OPERATOR (+)
    MODULE PROCEDURE vector_add
END INTERFACE
INTERFACE OPERATOR(-)
    MODULE PROCEDURE  vector_subtract
END INTERFACE
INTERFACE OPERATOR(*)
    MODULE PROCEDURE  vector_times_real
    MODULE PROCEDURE  real_times_vector
    MODULE PROCEDURE  vector_times_int
    MODULE PROCEDURE  int_times_vector
    MODULE PROCEDURE  vector_times_vector
END INTERFACE
!----------------------------------------!
!----------------操作函数----------------!
CONTAINS
!VECTOR +
TYPE(vec) FUNCTION vector_add(a,b)
    IMPLICIT NONE
    TYPE(vec),INTENT(IN)::a,b
    vector_add%x=a%x+b%x
    vector_add%y=a%y+b%y
    vector_add%z=a%z+b%z
END FUNCTION
!VECTOR -
TYPE(vec) FUNCTION vector_subtract(a,b)
    IMPLICIT NONE
    TYPE(vec),INTENT(IN)::a,b
    vector_subtract%x=a%x-b%x
    vector_subtract%y=a%y-b%y
    vector_subtract%z=a%z-b%z
END FUNCTION
!VEC *
! vector_times_real
TYPE(vec) FUNCTION vector_times_real(a,b)
    TYPE(vec),INTENT(IN)::a
    REAL,INTENT(IN)::b
    vector_times_real%x=a%x * b
    vector_times_real%y=a%y * b
    vector_times_real%z=a%z * b
END FUNCTION
! real_times_vector
TYPE(vec) FUNCTION real_times_vector(b,a)
    TYPE(vec),INTENT(IN)::a
    REAL,INTENT(IN)::b
    real_times_vector%x=a%x * b
    real_times_vector%y=a%y * b
    real_times_vector%z=a%z * b
END FUNCTION
! vector_times_int
TYPE(vec) FUNCTION vector_times_int(a,b)
    TYPE(vec),INTENT(IN)::a
    INTEGER,INTENT(IN)::b
    vector_times_int%x=a%x * b
    vector_times_int%y=a%y * b
    vector_times_int%z=a%z * b
END FUNCTION
! int_times_vector
TYPE(vec) FUNCTION int_times_vector(b,a)
    TYPE(vec),INTENT(IN)::a
    INTEGER,INTENT(IN)::b
    int_times_vector%x=a%x * b
    int_times_vector%y=a%y * b
    int_times_vector%z=a%z * b
END FUNCTION
!vec_times_vec
REAL FUNCTION vector_times_vector(a,b)
    TYPE(vec),INTENT(IN)::a
    TYPE(vec),INTENT(IN)::b
    vector_times_vector=a%x * b%x+a%y * b%y+a%z * b%z
END FUNCTION

END MODULE vectors

!----------测试程序---------------！
PROGRAM test_operator
    USE vectors
    IMPLICIT NONE
    TYPE(vec)::a
    TYPE(vec)::b,c
    !-----------接口块--------------!
    INTERFACE
        SUBROUTINE mulit(a,b,c)
            !从模块中导入变量类型
            !------------------------!
            IMPORT vec
            !------------------------!
            TYPE(vec),INTENT(IN)::a
            TYPE(vec),INTENT(IN)::b
            TYPE(vec),INTENT(OUT)::c
        END SUBROUTINE
    END INTERFACE
    !-------------------------------!
    READ(*,*)a,b
    WRITE(*,*)'a= ',a
    WRITE(*,*)'b= ',b
    WRITE(*,*)'ADD: ',(a+b)
    WRITE(*,*)'sub: ',(a-b)
    WRITE(*,*)'dot: ',(a*b)
    CALL mulit(a,b,c)
    WRITE(*,*)'mulit: ',c
CONTAINS


END PROGRAM
SUBROUTINE mulit(a,b,c)
TYPE subvec
   REAL::x
    REAL::y
    REAL::z
END TYPE
    TYPE(subvec),INTENT(IN)::a
    TYPE(subvec),INTENT(IN)::b
    TYPE(subvec),INTENT(OUT)::c
    c%x=a%x*b%x
    c%y=a%y*b%y
    c%z=a%z*b%z
END SUBROUTINE
```

## 12.8 绑定赋值符和操作符

通过使用GENERIC语句，**赋值符**和**操作符**都可以**和派生数据类型绑定**。

格式:

```fortran
TYPE :: point
REAL :: x
REAL :: y
CONTAINS
GENERIC :: ASSIGNMENT(=) => assignl
GENERIC :: OPERATOR(+) => plusl, plus2, plus3
END TYPE point
```

像在前一节定义通用赋值符和操作符一样，必须用同样的方法声明实现操作符的过程体。

## 12.9限制对模块内容的访问l

当使用USE关联访问模块时，默认情况下，模块中定义的所有实体对于含有USE语句的程序单元都可使用。

在12.7中，创建了一个名为vectors的模块， 该模块扩展了Fortran语言。任何访问模块vectors的程序单元可以定义自己的矢量，并且可通过使用二元操作＋、-、＊操作处理矢量。

程序也可以调用像vector_add、vector_subtract等这样的函数，不幸的是，只能通过使用定义操作符间接地调用它们。任何程序单元**不需要这些过程名**，但它们已被声明，而且**它们有可能和程序中定义的过程名冲突**。当许多数据项同时定义在一个模块中，但特定的程序单元仅需要少数几个数据项时，同样的问题也可能发生。这些非必要数据项在程序单元中均可访问，这样会导致程序员有可能错误的修改它们。

### 12.9.1  PUBLIC、PRIVATE和PROTECTED属性和语句

限制模块内容的访问：用PUBLIC、PRIVATE和PROTECTED属性和语句。

1. PUBLIC属性：**模块外的程序单元就可以访问该项。**
2. PRIVATE属性：**模块外的程序单元就不能访问该项**，但是模块中的过程仍可以访问该项。
3. PROTECTED属性：该项对于**模块外程序单元只可读**。任何除了定义该项之外的其他模块试图修改PROTECTED变量值都会引起编译错误。
4. 模块中所有数据和过程的默认属性是PUBLIC，因此，在默认情况下任何使用模块的程序单元都能访问模块中的每一个数据项和过程。

声明方式：

1. 在类型定义语句中将此状态作为属性指明

   ```fortran
   INTEGER,PRIVATE::count
   REAL,PUBLIC :: voltage
   REAL,PROTECTED :: my_data
   ```

   此类声明可以用于数据项和函数，但是不能用于子例程。可使用PUBLIC, PRIVATE或PROTECTED语句指明数据项、函数和子例程的状态。

2. 使用独立的Fortran 语句声明

   ```fortran
   PUBLIC :: list of public items
   PRIVATE :: list of private items
   PROTECTED :: list of private items
   ```

   如果一个模块包括PRIVATE语句而没有具体的内容列表，那么默认状态下，模块中每个数据项和过程都是私有的。任何公用项都必须使用单独的PUBLIC语句显式声明。

隐藏外部程序单元不需要直接访问的模块数据项或过程是一个很好的编程习惯。最好的方法是在每个模块中包含PRIVATE语句，然后将想要暴露出来的特定项使用单独的PUBLIC语句罗列出来。

示例：限制12.7中对函数的访问,只允许访问数据类型和操作符

```fortran
PRIVATE
PUBLIC::OPERATOR(+),OPERATOR(-),OPERATOR(*),vec
```

### 12.9.2 模块中派生数据类型的PUBLIC和PRIVATE声明

1. 派生数据类型元素对于模块外的程序单元可以设置为不可访问

   ```fortran
   TYPE vec
   	PRIVATE
   	REAL::x
   	REAL::y
   	REAL::z
   END TYPE
   ```

2. 将派生数据类型整体声明为私有的，仅适用于模块内部的运算

   ```fortran
   TYPE,PRIVATE::vect
   REAL :: x
   REAL :: y
   REAL :: z
   END TYPE
   ```

3. 将派生数据类型的单个元素声明为私有或公有

   ```fortran
   TYPE : : vec
   REAL,PUBLIC :: x
   REAL,PRIVATE :: y
   END TYPE
   ```

4. 派生数据类型本身是公有的，仍然可以将该类型的某个变量声明为私有

   ```fortran
   TYPE :: vec
   REAL :: x
   REAL :: y
   END TYPE
   TYPE(vec),PRIVATE :: vec_1
   ```
   
5. 声明某个变量的值在其定义的模块之外是只读的

   ```fortran
   REAL,PROTECTED::PI=3.1415925
   ```

## 12.10 USE语句的高级选项

当程序单元通过使用USE关联访问模块时， 在默认情况下该程序能访问模块中的每个数据项、接口和过程。可以通过把某些数据项声明为PRIVATE来限制程序的访问。除了这种方法之外， 还可以对使用模块的程序单元进一步限定所使用的数据项表， 并且修改这些数据项的名字。

### 12.10.1 限制对模块中特定数据项的访问

限制对模块中特定数据项的访问，可以将ONLY子句添加到USE语句中。

格式：

```fortran
USE module,ONLY:only_list
```

only_list是模块中要使用的数据项表， 数据项之间用逗号分开。

示例：

```fortran
USE vec,ONLY:operator(*),operator(+)
```

在包含此语句的过程中， 声明vec类型的变量以及乘法和加法操作是合法的， 但是两个矢量减法就是非法的。

### 12.10.2 重命名数据项或过程

同样可以在USE语句中重命名数据项或过程。

格式：

```fortran
USE module_name, rename_list
!程序单元可以访问模块中所有的公有项，但是rename_list中的那些数据项会被重命名。
USE module_name, ONLY: rename_list
!只可以访问列出来的数据项，也可以重命名它们。
```

rename_list中的每个数据项格式：

```fortran
Local_name => Module_name
!将模块中的名称Module_name改为Local_name
```

示例：模块data_fit中限制只能访问子例程sp_real_least_squares_fit并被重命名为lsqfit

```fortran
USE data_fit，ONLY:lsqfit => sp_real_least_squares_fit
```

如果有一个以上的USE语句引用了同一模块，那么下面的规则适用。

1. 如果没有USE语句重命名列表或ONLY子句，那么该语句只是彼此的复制，这是合法的，但是对程序没意义。
2. 如果所有的USE语句都包括重命名列表，并且没有ONLY子句，那么效果同于所有的重命名项列在单独的USE语句中。
3. 如果所有的USE语句都包括ONLY子句，那么效果同于所有的列表都列在了单独的USE语句中。
4. 如果有些USE语句有ONLY子句而有些没有，那么ONLY子句对程序没有什么影响。因为没有ONLY子句的USE语句允许所有模块中的公有项对于程序单元都是可见的，才会产生这个现象。

好的代码应该永不存在这类问题。

## 12.11 内置模块

Fortran 定义了一个概念“内置模块＂。内置模块就像普通的Fortran模块一样，是由Fortran编译器的创造者预定义和编写的。内置模块和普通模块一样，都是通过使用USE语句访问其过程和数据。

Fortran有很多标准的内置模块， 三个最重要的模块是：

1. 模块ISO_FORTRAN_ENV

   包含了描述特定计算机中存储器特性的常量（标准整型数包含多少bit, 标准字符包含多少bit 等）以及该机器所定义的I/O单元。

2. 模块ISO_C_BINDING

   包含了Fortran编译器和特定处理器的C语言互操作时所需的必要数据。

3. IEEE模块

   描述了IEEE754关于特定处理器上的浮点数运算的特征。标准的IEEE模块是IEEE_-EXCEPTIONS、IEEE_ARITHMETIC和IEEE_FEATURES。

Fortran标准需要编译器零售商实现内置模块中的特定过程，但是也允许零售商添加额外的过程，也允许定义它们自己的内置模块。以后，这种方法会成为给编译器增加新功能的一个常用方法。

## 12.12访问命令行参数和环境变量

### 12.12.1 命令行参数的访问

1. 函数COMMAND_ARGUMENT _COUNT()。

   这一函数返回启动程序的**命令行中参数个数**， 返回的默认类型是**整型数**。此函数没有参数。

2. 子例程GET _COMMAND ([COMMAND，LENGTH, STATUS])

   这个子例程返回命令行**参数的完整集合**，字符变量COMMAND中保存了该集合数据，整型变量LENGTH中保存了参数字符串的长度，整型变量STATUS存储了操作是否成功的状态。如果执行成功， 那么STATUS为0。如果字符串变量COMMAND太短放不下参数，那么STATUS将为－l。其他错误都会返回非零数据。所有的这些参数都是可选的， 所以用户可以通过使用关键字语法来指明所需的参数。

3. 子例程GET_COMMAND_ARGUMENT (NUMBER[, VALUE, LENGTH, STATUS])。

   - NUMBER(输入)：默认为整数(0<=NUMBER <=COMMAND_ARGUMENT _COUNT())，其中N=0时表示命令本身，是**可执行程序的名字**；若总的参数个数比NUMBER小，则返回空。

   - VALUE(输出，可选)：是相应的值； 默认为标量，字符型。

   - LEBGTH(输出，可选)：是第NUMBER    个参数的长度；标量整型。

   - STATUS(输出，可选)：是获取这个参数后的状态（如果取这个参数错误，返回的是正数；如果VALUE是一个 truncated的参数，那么返回-1；其它情况返回0）

示例：

```fortran
PROGRAM get_cornmand_line
!声明局部变量
INTEGER :: i                    !循环控制变量
CHARACTER(len=l28) :: command 	!命令行
CHARACTER(len=80) :: arg 		!单个参数
!获得程序名
CALL get_command_argument(0, command)
WRITE(*,'(A, A)')'Program name is: ', TRIM(command)
!在此获得单个参数
DO i = 1,command_argument_count()
CALL get_command_argument(i,arg)
WRITE(*,'(A,I2,A,A)')'Argument ',i,' is',TRIM(arg)
END DO
END PROGRAM get_cornmand_line

!输出
Program name is: get_command_line
Argument 1 is 1
Argument 2 is sdf
Argument 3 is 4
Argument 4 is er4
```

### 12.12.2 获取环境变量

```fortran
CALL GET_ENVIRONMENT_VARIABLE(NAME[,VALUE,LENGTH,STATUS,TRIM_NAME])
```

- NAME(INPUT)：用户提供的**字符表达式**， 用于容纳目标环境变量名
- TRIM_NAME(INPUT)：逻辑输入参数。如果它为真，那么命令行在与环境变量比较时会忽略末尾空格符。如果它为假，那么在比较时包括对末尾空格符的处理。

- VALUE(OUTPUT)：返回环境变量的值(地址?)，字符型。
- LENGTH(OUTPUT)：返回环境变量的长度，整型。
- STATUS(OUTPUT)：返回操作成功与否，整型。恢复成功，STATUS的值为0。如果字符变量VALUE太短放不下参数，那么STATUS的值为-l。如果环境变量不存在，STATUS的值为1。如果发生了其他错误，STATUS的值大于2。

示例：程序获取和显示环境变量windir的值

```fortran
PROGRAM get_env
!声明局部变撼
INTEGER :: length
INTEGER :: status
CHARACTER(len=80) :: value
!获得“wind立“环境变量值
CALL GET_ENVIRONMENT_VARIABLE('windir',value,length,status)
!告诉用户
WRITE(*, *) 'Get "windir" environment variable:'
WRITE(*,'(A,I6)')'Status =',status
IF ( status <= 0 ) THEN
WRITE(*,'(A,A)')'Value =',TRIM(value)
END IF
END PROGRAM get_env

!输出：
!Get 'windir' environment variable:
!Status = 0
!Value = C:\WINDOWS
```

使用标准Fortran内置的过程来获取启动程序的命令行参数以及环境变量的值，而不要使用个别编译器零售商提供的非标准过程。

## 12.13 VOLATILE属性和语句

当Fortran编译器为了发布而编译程序时，通常会采用优化选项来提高程序的速度。优化器采用了许多提高程序速度的技术， 但是一个十分常用的方法是在使用过程中将变量的值放到**CPU的寄存器**中， 因为对寄存器的访问远远快于对内存的访问。当有空闲的寄存器可以容纳数据时， 这是对于需要不断修改DO循环中变量的常用方法。

如果被使用的变量同样被Fortran程序之外的其他进程**访问或修改**，那么这种优化可能会引起严重的问题。这种情况下，当Fortran程序**正在使用一个与以前存储在寄存器不同的值时**，外部进程可能修改该变量的值。

为了避免数据不一致的情况， **数据必须保存在一个且仅一个位置上**。Fortran 编译器绝不能在寄存器中保存变量的备份，也不能因变量的值发生变化就更新主存。为了实现这点，可以将变量声明为**volatile （可变的）**。如果变量是volatile 的，那么编译器就不对它实行任何优化，程序直接使用主存中的该变量。

VOLATILE 属性或语句将变量声明为VOLATILE的，那么编译器就不对它实行任何优化，程序**直接使用主存中的该变量**。

```fortran
REAL, VOLATILE :: x
REAL, VOLATILE :: y
```

Volatile 语句形式如下：

```fortran
REAL :: x,y
VOLATILE :: x,y
```

VOLATILE 属性或语句通常用于大规模并行包处理中， 这样就有办法**在进程之间异步传输数据**。

## 12.14 小结

### 12.14.1 遵循原则

1. 当使用嵌套作用域时，避免对内外层作用域中同名的对象赋予不同的含义。这条准则特别适用于内部过程。通过给内部过程的变量赋予与主过程不同的名字，就可以避免两者变量行为的混淆。
2. 只要可能，尽量使用**模块中的过程**取代接口块。
3. 如果必须为很多过程创建接口，那么将所有这些接口放在一个模块里，这样程序单元就可以很容易地通过USE关联访问这些接口。
4. 用户自定义通用过程可以用于定义适用于不同输入数据类型的过程。
5. 接口操作符块和接口赋值符块可以用于创建新的操作符，可以扩展用于派生数据类型的已有操作符的含义。一旦定义了合适的操作符，使用派生数据类型就非常容易了。
6. 隐藏外部程序单元不需要直接访问的模块数据项或过程是良好的编程习惯。实现它最好的方法是在每个模块中都包含PRIVATE语句，然后将需要放开访问权的特定数据项单独使用PUBLIC语句修饰。
7. 使用标准的Fortran内置过程获取启动程序的命令行参数和环境变量，而不要使用个别编译器零售商所提供的非标准过程。

### 12.14.2 语法小结

1. **BLOCK结构体**

   ```fortran
   !格式：
   [name:] BLOCK
   ... variable declarations
   ...
   Executable statements
   ...
   IF() EXIT [name]
   ...
   END BLOCK [name]
   !示例
   
   ```

   BLOCK结构体是位于主程序或者过程内的一段代码块， 它可以**定义自己的局部变量**，**也可以通过宿主关联访问其宿主的变量**。当执行语句跳出块后，这些局部变量变成未定义状态。

3. **GENERIC语句**

   ```fortran
   !格式：
   TYPE [::] type_ name
   compqnent 1
   ...
   component n
   CONTAINS
   	GENERIC :: generic_name => proc_namel [, proc_name2, ... ]
   END TYPE [type_name]
   !示例：
   TYPE :: point
   REAL :: x
   REAL :: y
   CONTAINS
   GENERIC :: add => point_plus_point,point_plus_scalar
   END TYPE point
   ```

   GENERIC语句定义了派生数据类型的**通用绑定**。和通用过程相关的特定过程在操作符=>之后列出。

4. **通用接口模块**

   ```fortran
   !格式
   INTERFACE generic_name
   	interface body_1
   	interface body_2
   END INTERFACE
   !示例：
   INTERFACE sort
   MODULE PROCEDURE sortint
   MODULE PROCEDURE sortreal
   END INTERFACE
   ```

   通过使用通用接口块声明通用过程。通用接口块在第一行声明了**通用过程的名字**，然后在接口体中列出了与通用过程关联的特定过程的显式接口。显式接口必须由不在模块中的特定过程定义。出现在模块中的过程使用带有MODULE PROCEDURE语句来引用，因为这些过程的接口是已知的。

5. **IMPORT语句**

   ```fortran
   !格式：
   IMPORT var_namel [,var_name2,...]
   !示例：
   IMPORT::x,y
   ```

   IMPORT语句将过程中的**类型定义**导入到接口定义里。Fortran2003引入了IMPORT语句，允许**接口声明中**引用**接口所在模块内的相关定义**。IMPORT 语句**指定主机作用域单元中的命名实体**可在接口中访问.以这种方式导入并在宿主范围单元中定义的实体应在**接口主体之前显式声明**.

6. **接口赋值块**

   ```fortran
   !模块：
   INTERFACE Assignment (=)
   	interface_body
   END INTERFACE
   !示例：
   INTERFACE ASSIGNMENT (=)
   	MODULE PROCEDURE vector to array
   	MODULE PROCEDURE array_to_vector
   END INTERFACE
   ```

   接口赋值块用于扩展赋值符号的含义，以便实现两个不同派生数据类型变量，或派生数据类型和内置数据类型之间的赋值操作。接口体中的每个过程必须是带有两个参数的子例程。第一个参数必须具有INTENT (OUT)属性，第二个参数必须具有INTENT(IN)属性。接口体中的所有子例程必须可以根据参数的顺序和类型区分开。

7. **接口块**

   ```fortran
   !格式
   INTERFACE
   interface_body_1
   END INTERFACE
   !示例
   INTERFACE
   	SUBROUTINE sort(array,n)
   	INTEGER,INTENT(IN):: n
   	REAL,INTENT(INOUT),DIMENSION(n):: array
   	END SUBROUTINE
   END INTERFACE
   ```

   接口块用于**为独立编译的过程声明显式接口**。它既可出现在希望调用独立编译过程的过程的头中，也可以出现在模块中，希望调用独立编译过程的过程可以使用这个模块。

8. **接口操作符块**

   ```fortran
   !格式：
   INTERFACE OPERATOR(operator_ symbol)
   	interface_body
   END INTERFACE
   !示例：
   INTERFACE OPERATOR (*)
   	MODULE PROCEDURE real_times_vector
   	MODULE PROCEDURE vector_times_real
   END INTERFACE
   ```

   接口操作符块用来定义新操作符，或扩展内置操作符的含义，以便支持派生数据类型。接口中的每个过程必须是参数属性为INTENT (IN)的函数。如果操作符是二元的， 那么函数必须有两个参数。如果是一元操作符， 那么函数必须只有一个参数。接口体中的所有函数必须能通过参数的类型和顺序区分开。

9. **MODULE PROCEDURE语句**

   ```fortran
   !格式
   MODULE PROCEDURE module_procedure_l[,module_procedure_2,...]
   !示例
   INTERFACE sort
   MODULE PROCEDURE sorti
   MODULE PROCEDURE sortr
   END INTERFACE
   ```

   MODULE PROCEDURE语句用在接口块中，**用来指明包含在模块中的过程**将和该接口定义的**通用过程**、**操作符**或**赋值符**相关联。

10. **PROTECTED属性**

    ```fortran
    !格式
    type, PROTECTED:: namel[,name2,... ]
    !示例
    INTEGER,PROTECTED:: i_count
    REAL,PROTECTED:: result
    ```

    PROTECTED属性声明了变量的值在其定义的模块之外是只读的。该值在通过USE语句访问定义模块的过程中只能使用而不能修改。

11. **PROTECTED语句**

    ```fortran
    !格式
    PROTECTED :: namel[,name2,...]
    !示例
    PROTECTED :: i_count
    ```

    PROTECTED语句声明了变量的值在其定义的模块之外是只读的。该值在通过USE语句访问定义模块的过程中只能使用而不能修改。

12. **递归FUNCTION语句**

    ```fortran
    !格式
    RECURSIVE [type] FUNCTION name(argl[, arg2, ... ]) RESULT (res)
    !示例
    RECURSIVE FUNCTION fact(n) RESULT (answer)
    INTEGER :: answer
    ```

    此语句声明递归Fortran 函数。递归函数是可以调用自己的函数。函数的类型要么在FUNCTION语句中声明， 要么在单独的类型声明语句中声明（声明的是结果变量res的类型， 不是函数名的类型）。函数调用所返回的结果是赋给函数体中res变量的值。

13. **USE语句**

    ```fortran
    !格式
    USE module name (,rename_list, ONLY: only_list)
    !示例
    USE my—procs
    USE my—procs,process_vector_input => input
    USE my_procs,ONLY:input => process_vector_input
    ```

    USE语句可以让USE语句所在的程序单元访问命名模块的内容。除了这个基本功能之外，USE语句允许用户将模块对象重命名为自己需要的名字。ONLY子句允许程序员指定该程序单元只能访问模块中的特定对象。

14. **VOLATILE属性**

    ```fortran
    !格式
    type, VOLATILE :: namel [,name2, ... ]
    !示例
    INTEGER,VOLATILE :: I_count
    REAL,VOLATILE :: result
    ```

    VOLATILE属性声明可以在任何时候用程序外部的资源修改变量的值，因此所有该变量的读操作必须直接从内存中读，所有对该变量的写操作也必须直接写到内存，而不是写到缓存副本中。

15. **VOLATILE语句**

    ```fortran
    !格式
    VOLATILE :: namel[,name2, ... ]
    !示例
    VOLATILE :: x,y
    ```

    VOLATILE语句声明可以在任何时候用程序外部的资源修改变量的值，因此所有该变量的读操作必须直接从内存中读，所有对该变量的写操作也必须直接写到内存，而不是写到缓存副本中。













