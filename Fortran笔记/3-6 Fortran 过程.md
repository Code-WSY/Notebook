# 6 过程

Fortran中有两种外部过程：子例程(subroutine)和函数子程序(function subprogram)(或者叫函数)

- 子例程：通过在一个单独的CALL语句中引用其名称进行调用的过程，并且可以通过调用参数来返回多个结果。

- 函数：通过在表达式中引入函数名来进行调用的过程，返回单个数值，该值用来参与表达式求值。

在实际编程时，把一个大程序任务分解为若干个子过程，可以荻得很多好处：独立的组件测试、复用性以及隔离无意的副作用。

## 6.1 子例程(SUBROUTINE)

一般格式：

```fortran
SUBROUTINE subroutine_name (argument_list)
	...
	(Declaration section)
	...
	(Excution section)
	...
RETURN
END SUBROUTINE [subroutine_name]
```

每个子例程是个独立的程序单元，它开始于SUBROUTINE 语句，结束于END SUBROUTINE语句。它的编译也独立于主程序和其他的过程。因为程序中的每个程序单元都是**独立进行编译**的，**局部变量名**和**语句标号**可以在不同的例程(routine)中被**复用**， 且不会引起任何错误。

任何可执行程序单元都可以调用子例程，包括另 个子例程（但是，子例程不能调用它自身，除非它被定义为递归类型(recursive) ）

### 6.1.1 **调用子例程(CALL语句)：**

```fortran
CALL subroutine_name(argument_list)
```

这里，参数列表中的实际参数的顺序和类型必须和子例程中定义的形参的顺序和类型相匹配。

例：

```fortran
!勾股定理
SUBROUTINE calc_hypotenuse(side_1,side_2,hypotenuse)
IMPLICIT NONE
!数据字典：声明调用参数类型和定义
REAL,INTENT(IN)::side_1
REAL,INTENT(IN)::side_2
REAL,INTENT(OUT)::hypotenuse
!数据字典：声明局部变量名和类型
REAL::temp
temp=side_1**2+side_2**2
hypotenuse=SQRT(temp)
END SUBROUTINE calc_hypotenuse
```

### 6.1.2 INTENT属性

INTENT(IN)：声明输入值，仅用于向子程序传递输入数据

INTENT(OUT):声明输出变量，仅用于将结果返回给调用程序

INTENT(INOUT) or INTENT(IN OUT):声明输入输出变量。即用于向子程序输入数据，也用来向调用程序返回结果。

> 在每一个过程中要始终记得声明每一个形参的INTENT属性

为了调用子例程，需要写驱动程序来测试：

```fortran
PROGRAM test_calc_hypotenuse
IMPLICIT NONE
!数据字典：声明变量类型和定义
REAL::s1     !边长1
REAL::s2     !边长2
REAL::hypot  !斜边
!获得两个边的长度
WRITE(*,*)'Program to test subroutine calc_hypotenuse:'
WRITE(*,*)'Enter the length of side 1:'
READ(*,*)s1
WRITE(*,*)'Enter the length of side 2:'
READ(*,*)s2
!调用calc_hypotenuse
CALL calc_hypotenuse(s1,s2,hypot)
!输出斜边
WRITE(*,1000)hypot
    1000 FORMAT('The length of the hypotenuse is: ',F10.4)
END PROGRAM test_calc_hypotenuse
```

### 6.1.3 Fortran中的变量传递：地址传递方案

Fortran程序和它的子例程之间用地址传递(pass-by-reference)方案来进行通信：

当调用子例程时，主程序传递一个**指针**来指向**实参表**中各个参数的存储位置。 子例程查找调用程序所指向的内存位置， 以获得它需要的形参值。 

- 必须确保**调用参数列表中的值**与**子例程的调用参数**在**个数**、 **类型**、 **次序**方面都**完全匹配**。

### 6.1.4 传递数组给子例程

为确保子例程知晓数组的大小，有三种方式可指明形参数组的大小：

1. 在子例程调用时，将数组每一维度的边界值作为参数传递给子例程，并且将相应的形参数组声明为该长度。

```fortran
!声明数组data1、data2，它们的宽度为n,然后在数组中处理nvals个元素值， 如果子例程中发生了引用越界错误，可以被检测到并报告。
SUBROUTINE process(data1,data2,n,nvals)
INTEGER,INTENT(IN)::n,nvals
REAL,INTENT(IN),DIMENSION(n)::data1
REAL,INTENT(OUT),DIMENSION(n)::data2
DO i= 1,nvals
data2(i)=3.*data(i)
END DO
END SUBROUTINE process
```

   

2. 把子例程中的所有形参数组声明为不定结构的形参数组， 以创建一个子例程的显式接口。

3. 第三种（也是最古老的）一种方法，用星号（＊）来声明每一个形参数组的长度， 称为不定大小的形参数组。(这种方式写出的子例程很难调试, 这种子例程也不能操作整个数组或部分数组 )

- 永远不要在子例程中使用 **STOP** 语句。 如果这么做，可能发布给用户一个一旦遇到某些特定数据集就会神秘终止的程序。
- 如果在一个子例程中可能存在错误条件，那么应该对错误进行检测，并设置错误标志，以返回给调用程序。调用程序在调用子例程后，应该对错误条件进行检测，并采取适当的 操作。

## 6.2 用模块共享数据(MODULE)

### 6.2.1 模块的基本形式

模块是个独立编译的**程序单元**，它包含了希望在程序单元间**共享**的数据的**定义**和**初始**值。

如果程序单元中使用了包含模块名的USE语句，那么在该程序单元中可以使用**模块中声明的数据**。每个使用同一个模块的程序单元可以访问同样的数据，所以模块提供了一种程序单元间的共享数据的方式。

**模块**格式：

```fortran
MODULE shared_data
!
!目的：声明在两个程序之间共享的数据
IMPLICIT NONE
SAVE
INTEGER,PARAMETER::num_vals= 5 !数组中的最大数值个数
REAL,DIMENSION(num_vals)::values  !数值值
END MODULE shared_data
```

### 6.2.2 SVAE和USE语句

**SAVE**:确保模块中声明的数据值在不同过程间引用时会被保留。**在任何声明了可共享数据的模块中都应包含这条语句。**

**USE**:要使用模块中的数值，程序单元必须使用USE语句声明模块名，格式：

```fortran
USE module_name
```

**USE 语句必须出现在程序单元中的其他任何语句之前**。

```fortran
PROGRAM test_module
USE shared_data
IMPLICIT NONE
REAL,PARAMETER::PI=3.141592 !Pi
values=PI*[1.,2.,3.,4.,5.]
CALL sub1 !调用子程序
END PROGRAM test_module
!*****************************
!*****************************
SUBROUTINE sub1
USE shared_data
WRITE(*,*)values
END SUBROUTINE sub1
```

声明局部变量时，它的名字不应该和从关联 USE 继承的变量同名。这种对变量名的重复定义将会产生编译错误。

## 6.3 模块过程(CONTAINS)

除了数据之外，模块还可以含有完整的子例程和函数，它们被称为模块过程。

### 6.3.2 CONTAINS语句

**CONTATINS**:告诉编译器后面得语句被包含在过程中。

这些过程被作为模块的一部分进行编译，并且可以通过在程序单元中使用包含模块名的USE 语句使模
块过程在程序单元中有效，例如：

```fortran
!建立一个模块
MODULE my_subs
IMPLICIT NONE
!（在这里声明共享数据）
CONTAINS
	SUBROUTINE subl(a,b,c,x,error)
	IMPLICIT NONE
	REAL,DIMENSION(3),INTENT (IN)::a
	REAL,INTENT(IN)::b,c
	REAL,INTENT(OUT)::x
	LOGICAL,INTENT(OUT)::error
	END SUBROUTINE subl
END MODULE my_subs
```

如果程序单元的第一个非注释语句是“ USE my_subs"，那么调用程序单元中可以使用子例程subl。

```fortran
PROGRAM main_prog
USE my—subs
IMPLICIT NONE
CALL subl(a,b,c,x,error)
END PROGRAM main—prog
```

### 6.3.2 使用模块创建显式接口

一个在模块内编译和用USE 访问的过程称为**带有显式接口**(explicit interface)， 因为无论何时使用过程， Fortran编译器都清楚地知道过程的每个参数的所有细节， 并可以通过检查接口来确保正确使用过程。

例如：

```fortran
MODULE my_subs
CONTAINS
	SUBROUTINE bad_argument(i)
	IMPLICIT NONE
	INTEGER,INTENT(IN)::i   !声明参数为整型
	WRITE(*,*) 'I = ',i !输出i
	END SUBROUTINE
END MODULE my_subs
!*******************************************************************
!*******************************************************************

PROGRAM bad_call
!目的：演示错误解释的调用参数
USE my_subs
IMPLICIT NONE
REAL:: x=1.
CALL bad_argument(x)
END PROGRAM bad_call
!OUTPUT:
!报错：
!bad_call2.f90(21) :error #6633:The type of the actual argument differs from
!the type of the dummy argument.[X]
!CALL bad_argument(x)
```

与之相反，不在模块内的过程称为**带有隐式接口**(implicit interface)。Fortran 编译器在编译调用过程的程序单元时，不知道这些过程的任何信息， 所以只能假设程序员正确地使用了参数的个数、类型、方向等信息。如果程序员事实上使用了错误的调用参数序列，那么程序将莫名其妙地终止，而且很难找到原因。

例如：

```fortran
PROGRAM bad_call
!目的：
!演示错误的解释调用列表
IMPLICIT NONE
REAL:: x = 1.                !声明实数变扯x
CALL bad_argument(x)         !调用子程序
END PROGRAM bad_call

SUBROUTINE bad_argument (i) 
IMPLICIT NONE
INTEGER::i                   !声明整数参数
WRITE(*,*)'i= ',i
END SUBROUTINE bad_argument

!OUTPUT:
!I = 1065353216
!内容错误，不会报错，很难找到原因
```

在过程中，要么使用**不定结构数组**，要么使用**显式结构数组**作为**形参数组参数**。如果使用不定结构数组，还需要一个**显式接口**。当用这两种方式声明数组形参时， 整个数组操作、数组部分， 以及数组的内部函数都可以使用。永远不要在任何新程序中使用不定大小的数组。



## 6.4 Fortran函数(FUNCTION)

### 6.4.1 Fortran函数的基本形式

Fortran 函数是这样个过程，它的结果是单个数值、逻辑值、字符串或数组。

一个函数得结果是单个数值或单个数组，它可以和变量、常量结合，以形参一个Fortran表达式。

类型：

1. 内部函数(intrinsic function)

   内建在Fortran语言中的函数，例如：SIN(X),LOG(X)

2. 用户自定义函数(user-defined function)

   用户定义函数由程序员定义， 用来满足标准内部函数无法解决的特定需求。

   通用格式：

```fortran
FUNCTION name(parameter_list)
...
!在声明部分必须声明name的类型
...
!执行部分
...
name=expr
RETURN
END FUNCTION [name]
```


   在函数中，函数名必须至少出现在赋值语句的左侧一次。 当返回调用程序单元时，赋给函数名的值是函数的返回值。

   由于函数能够返回一个值，所以必须为其指定一个类型。如果使用了IMPLICIT NONE语句，那么在**函数过程**和**调用程序**中都应该声明函数类型。如果没有使用IMPLICIT NONE语句，除非用类型声明语句覆盖，否则函数的缺省类型将会遵循Fortran语言的标准规则来确定。

   一个用户定义Fortran函数的类型声明可以采用以下两种等价格式来完成：

   ```fortran
   INTEGER FUNCTION my_function(i,j)
   ```

   ```fortran
   FUNCTION my_function(i,j)
   INTEGER::my_function
   ```

   对于函数类型的声明，举例：

   ```fortran
   REAL FUNCTION quadf(x,a,b,c)  !声明函数类型
   IMPLICIT NONE
   REAL,INTENT(IN)::x
   REAL,INTENT(IN)::a
   REAL,INTENT(IN)::b
   REAL,INTENT(IN)::c
   !表达式求值
   quadf=a*x**2+b*x+c
   END FUNCTION quadf
   ```

   调用函数：

   ```fortran
   PROGRAM test_quadf
   IMPLICIT NONE
   !数据字典：声明变量的类型和定义
   REAL::quadf
   REAL::a,b,c,x
   !获得输入数据：
   WRITE(*,*)'Enter quadratic coefficients a,b,and c:' 
   READ (*,*)a,b,c 
   WRITE(*,*)'Enter location at which to evaluate equation:' 
   READ (*,*)x
   !输出结果
   WRITE(*,100)'quadf(',x,')= ',quadf(x,a,b,c) 
   100 FORMAT (A,F10.4,A,F12.4)
   END PROGRAM test_quadf
   ```

   - 要在用户定义函数本身和任何调用该函数的其他程序中声明函数类型。

     

3. 函数子程序(function subprograms)

   用户定义函数由程序员定义， 用来满足标准内部函数无法解决的特定需求。

### 6.4.2 函数中的意外副作用

​		输入数据通过函数的**参数表**传递给函数， 函数和子例程使用的是**同样**的**参数传递模式**。函数获取的是指向参数位置的**指针**，所以它可能会有意或无意地修改了那些内存位置中的内容。 因此，**一个函数子程序有可能修改了自己的输入数据**。

​		**如果函数的任何一个形参出现在了函数中赋值语句的左侧，那么对应那些参数的输入变量的值将会被改变**。修改了其参数表值的函数会产生副作用。

​		根据定义，函数用一个或多个输入值生成一个输出值， 且不应该有副作用。 **函数永远不应该修改自身的输入参数。** 如果程序员需要用一个过程生成多个输出值， 那么应该把过程写成为子例程而不是函数。**为了确保函数的参数不被无意地修改，应该总是用INTENT(IN)属性声明输入参数。**

## 6.5 过程作为参数传递给其他过程

### 6.5.1 用户定义函数作为参数传递

只有在调用和被调用过程将用户定义函数声明为**外部 (external)** 时，才能将其作为调用参数传递。当参数表中的某个名字被声明为外部时，相当于告诉编译器在参数表中传递的是独立的已编译函数，而不是变量。

**EXTERNAL属性**或者**EXTERNAL语句**均可以声明函数为外部的。

EXTERNAL 属性像其他属性一样包括在类型声明语句中：

```fortran
REAL,EXTERNAL::fun_1,fun_2
```

EXTERNAL语句是一个特殊语句，格式如下：

```fortran
EXTERNAL fun_1,fun_2
```

函数作为参数传递：

```fortran
PROGRAM::test
	REAL,EXTERNAL::fun_1,fun_2
	REAL::x,y,output
	CALL evaluate(fun_l,x,y,output）
	CALL evaluate(fun_2,x,y,output)
END PROGRAM test
!**************************
SUBROUTINE evaluate(fun,a,b,result)
	REAL,EXTERNAL::fun
	REAL,INTENT(IN)::a,b
	REAL,INTENT(OUT)::result
	result=b*fun(a)
END SUBROUTINE evaluate
```



### 6.5.2 子例程作为参数传递

子例程也可以作为调用参数传递给过程。如果要把子例程当作参数传递，也必须用EXTERNAL语句来声明它。相应的形参应该出现在过程中的CALL语句里。

举例：子例程subs_ as_ arguments 调用一个子例程，完成对x和y的处理。要执行的子例程名字作为命令行参数传递。

```fortran
SUBROUTINE subs_as_arguments(x,y,sub,result)
!目的： 测试将子例程名字作为参数
	IMPLICIT NONE
!数据字典：声明调用参数的类型和定义
	EXTERNAL::sub
	REAL,INTENT(IN)::x
	REAL,INTENT(IN)::y
	REAL,INTENT(OUT)::result
	CALL sub(x,y,result）
END SUBROUTINE subs_as_arguments
```

## 6.6 小结

### 6.6.1 遵循原则

当用子例程或函数时， 应该遵循下述原则：

1. 尽可能地将大程序任务分解为更小更易理解的过程。
2. 始终记得用INTENT 属性指定过程中每个形参的作用，以帮助捕捉编程错误。
3. 确保过程调用的实参表和形参表中的变量个数、类型、属性和位置次序都匹配。**把过程放在模块中**，然后用USE 访问该过程，将创建显式接口，这可以使编译器自动检测出参数表中是否有错。
4. 测试子例程中可能的错误条件，并设置错误标志，以返回给调用程序单元。当调用子例程之后，调用程序单元应该检查错误条件，以便在发生错误时采取相应操作。
5. 始终使用显式结构的形参数组或不定结构的形参数组。在新的程序中，永远不要使用不定大小的形参数组。
6. 模块可用来在程序的过程间传递大量数据。模块中的数据仅需要声明一次，所有的过程就可以通过那个模块来访问那些数据。要保证模块中含有SAVE 语句，以确保过程在访问模块后，其中的数据值被保留。
7. 把程序中要使用的过程集中起来，并放置在模块中。当它们构成模块的时候，Fortran编译器将会在每次使用它们的时候，自动检测调用参数表。
8. 确保在函数本身和任何调用该函数的程序单元中声明函数的类型。
9. 一个设计良好的Fortran 函数应根据一个或多个输入值生成单个输出值。它不应该修改自己的输入参数。为了保证函数不会偶然修改了自己的输入参数，**始终用INTENT (IN)属性声明参数**。

### 6.6.2 语法小结

**CALL语句:**

```fortran
CALL subname(argl, arg2,... )
!例如：
CALL sort(number,datal }
```

此语句将执行从当前程序单元传送到子例程，将指针传递给调用参数。子例程一直执行，直到遇到RETURN或END SUBROUTINE语句，然后在调用程序单元中的CALL 语句之后的下一个可执行语句处继续执行。

**CONTAINS语句:**

```fortran
CONTAINS
!例如：
MODULE test
	CONTAINS
	SUBROUTINE subl(x,y)
	END SUBROUTINE subl
END MODULE test
```

CONTAINS 语句指定以下语句是模块中的单独过程。CONTAINS 语句及其后的模块过程必须出现在模块中的任何类型和数据定义之后。

**END语句:**

```fortran
END FUNCTION [name]
END MODULE [name]
END SUBROUTINE [name]
!例如：
END FUNCTION my—function
END MODULE mymod
END SUBROUTINE mysub
```

**EXTERNAL属性:**

```fortran
type,EXTERNAL: :namel,name2,...
!例如：
REAL,EXTERNAL ::myfunction
```

该属性声明一个特定的名称是外部定义的函数。相当于在EXTERNAL语句中命名该函数。

**EXTERNAL语句:**

```fortran
EXTERNAL namel, name2,...
!例如：
EXTERNAL myfunction
```

该语句声明特定的名称是外部定义的过程。如果将EXTERNAL语句中指定的过程作为实参传递，则在调用程序单元和被调用过程中必须使用该语句或EXTERNAL属性。

**FUNCTION语句:**

```fortran
[type] FUNCTION name(argl,arg2,...)
!例如：
INTEGER FUNCTION max_value (num,iarray)
FUNCTION gamma(x)
```

此语句声明用户定义的Fortran函数。函数的类型可以在FUNCTION语句中声明，也可以在单独的类型声明语句中声明。该函数通过在调用程序中的表达式中命名来执行。形参是执行函数时传递来的调用参数的占位符。如果一个函数没有参数，在声明它的时候仍然必须使用一对空括号［例如name()]。

**INTENT属性：**

```fortran
type,INTENT(intent —type): :namel,name2,...
!例如:
REAL,INTENT(IN)::value
INTEGER,INTENT(OUT)::count
```

这一属性声明指定过程中特定形参的预期用途。intent_type 的可能值为IN, OUT 和INOUT。INTENT属性允许Fortran编译器知道参数的预期用途，并检查它是否以预期的方式使用。此属性只可能出现在过程的形参中。

**INTENT语句：**

```fortran
INTENT(intent_type)::namel,name2,...
!例如：
INTENT(IN)::a,b
INTENT(OUT)::result
```

此语句声明过程中特定形参的预期用途。intent_type的可能值为IN, OUT和INOUT。INTENT语句允许Fortran编译器知道参数的预期用途，并检查它是否以预期的方式使用。INTENT语句中只能出现形参。不要使用这个语句；请改用INTENT属性。

**MODULE语句：**

```fortran
MODULE name
!例如：
MODULE my_data_and_subs
```

这个语句声明了一个模块。该模块可能包含数据、过程或两者均有。通过在USE语句(USE关联）中声明模块名称， 数据和过程可用于程序单元。

**RETURN语句：**

```fortran
RETURN
!例如：
RETURN
```

当在一个过程中执行该语旬时，控制返回到调用该过程的程序单元。这个语句在子例程或函数结束时是可选的，因为当达到END SUBROUTINE 或END FUNCTION语句时，执行将自动返回到调用程序。

**SUNROUTION语句：**

```fortran
SUBROUTINE name (argl,arg2,...)
!例如：
SUBROUTINE sort(num,datal)
```

说明：
该语句声明了Fortran子例程。子例程用CALL语句执行。形参是执行子例程时传递的调用参数的占位符。

**USE语句：**

```fortran
USE modulel,modu1e2,...
!例如：
USE my_data
```

该语句使得一个或多个模块的内容可用于程序单元中。USE语句必须是程序单元中的PROGRAM, SUBROUTINE或FUNCTION语句后的第一个非注释语句。
