# 8 过程的附加特性

## 8.1 给子例程和函数传递多维数组

### 8.1.1 显示结构的形参数组

在这种方式中，将数组和数组中每一维度的取值范围传递给子例程。范围值用于在子例程中声明数组的大小， 这时子例程知道数组的所有信息。

```fortran
SUBROUTINE process1(data1,data2,n,m)
INTEGER,INTENT(IN):n,m
REAL,INTENT(IN),DIMENSION(n,m):data1    !显示结构
REAL,INTENT(OUT),DIMENSION(n,m):data2
data2=3.*data1
END SUBROUTINE process1
```

### 8.1.2 不定结构的形参数组

第二种方式是在子例程中声明所有的形参数组为不定结构的形参数组。

在声明不定结构的数组时，数组中的**每个下标都用冒号来代替**。**只有子例程或者函数有显式接口，才能使用这种数组**，于是调用程序知道关于子例程接口的所有信息。

因为编译器可以从接口中的信息判断每个数组的大小和结构， 所有对整个数组和部分数组的操作以及数组内置函数都可以使用不定结构的形参数组。如果有必要的话， 可以利用表7-1中的数组查询函数求出不定结构数组的实际大小和宽度。但是，因为只有实际数组的结构被传递给了过程， 而不包含每个维度的取值范围， **所以并不能够获知每个维度的上下标取值范围。**如果出于某种原因在特定的过程中必须要求实际的边界值， 那么就必须使用**显式结构**的形参数组。
如果不需要将每个边界从调用程序单元中传递给过程， 那么不定结构的形参数组通常要比显式结构的形参数组好用。但是如果过程中有显式接口，不定结构的数组才能工作正常。

通常采用的方式是**将子程序放在模块中**， 然后在调用程序中使用(USE)该模块：

```fortran
MODULE test_module
CONTAINS
	SUBROUTINE process2(data1,data2)
		REAL,INTENT(IN),DIMENSION(:,:)::data1
		REAL,INTENT(OUT),DIMENSION(:,:)::data2
		data2=3.*data1
	END SUBROUTINE process2

END MODULE test_module
```

## 8.2 SAVE 属性和语句

Fortran 提供了一种方式来保证在调用过程之间不修改保存的局部变量和数组：**SAVE 属性**。

像其他任何属性一样， SAVE 属性出现在类型声明语句中。在调用过程间，任何一个用SAVE属性定义的局部变量都会被保存，而不改变。

**任意在类型声明语句中初始化的局部变量都会被自动保存**，不论是否显式地指明了它的属性，变量的值都会被保存起来。如下，在过程中，两者作用相同。

```fortran
REAL,SAVE::sums
REAL::sum_x2=0
```

Fortran 也提供了SAVE 语句。它是一个位于过程的声明部分的**非执行语句**，**<u>跟在类型声明语句后面</u>**。任何列在SAVE 语句中的局部变量都会在调用过程间无改变地保存。

如果SAVE语句中没有变量， 那么所有的局部变量都会被无改变的保存起来。
```fortran
SAVE::var1,var2
or
SAVE
```
SAVE属性不能出现在**关联的形参**中或是**用PARAMETER 属性定义的数据项**中。同样，这些数据项也不能出现在SAVE语句中。

## 8.3 过程中的可分配数组

用于过程中的可分配数组在过程中应该声明为**局部变量**。如果用SAVE属性来声明可分配数组或者它出现在一个SAVE 语句中， 那么数组只会在该过程第一次被调用的时候利用AOOLCATE语句**分配一次内存**。然后数组会被用于计算，但**它的内容在每次调用过程间会被保持原封不动**。
如果没有用SAVE 属性来声明可分配数组，那么在每次调用过程的时候都必须用ALLOCATE语句给数组分配空间。数组还是会被用于计算，但是当返回到调用程序的时候，**数组的内容会被自动地释放掉**。

## 8.4 过程中的自动数组

自动数组：在过程执行的时候自动创建临时数组， 在过程执行到返回之后自动释放掉数组。

自动数组是**局部的显式结构数组**， 它**没有固定的下标取值**（**下标由形式参数或者来自于模块的数据指定**）。

示例：

```fortran
SUBROUTINE subl(x,y,n,m)
	IMPLICIT NONE
	INTEGER,INTENT (IN)::n,m
	REAL,INTENT(IN),DIMENSION(n,m)::x
	REAL,INTENT(OUT),DIMENSION(n,m)::x
	REAL,DIMENSION(n,m)::temp
	temp=0.
	...
END SUBROUTINE sub1
!数组temp是子例程中创建的自动数组(无声明INTENT属性)。
!当子例程开始运行的时候，自动地创建大小为nxm的数组temp,当子例程运行结束，数组被自动销毁。
```

为自动数组指定SAVE属性是非法的。

### 8.4.1 自动数组和可分配数组的比较

1. 当进入含有自动数组的子例程时，会自动地分配自动数组，而可分配数组必须手动的分配和释放空间。这个特性表明，自动数组可用于一个仅需要临时空间的过程中，用它可以调用其他的过程。

2.  可分配数组更通用和灵活。因为它们可以在独立的过程中创建和销毁。

   例如， 在一个大型的程序中，可以创建一个特定的子例程，来为所有的数组分配合适大小的空间，以解决当前问题，还可以创建另一个不同的子例程，在数组使用完后，释放它们。因此，可分配数组可以用千主程序，而自动数组不可以。

3. 在计算过程中，可分配数组可以改变大小。相反，自动数组是在过程执行开始时自动地分配指定大小的空间， 在执行过程中，数组的大小不能被改变。

   在执行的过程中，程序员可以使用DEALLOCATE和ALLOCATE语句。改变可分配数组的大小，所以在单个过程中， 单个数组能满足多个需要不同结构的数组的需求。自动数组通常被用于创建过程内部临时工作数组。而可分配数组被用于那些在主程序中创建，或在不同的过程中创建或销毁的数组，或者用于在给定过程中能够改变大小的数组。

### 8.4.2 Fortran不同数组类型

1. **带有常数下标的显示结构数组**

   ```fortran
   INTEGER,PARAMETER,NDIM=10
   REAL,DIMENSION(NDIM,NDIM)::input_data=1.
   REAL,DIMENSION(-3,3)::scratch=0.
   ```

2. **形参数组**

   形参数组（也称为**哑元数组**）是出现在过程的**形式参数列表**中的数组。

   - 显示结构形参数组

     ```fortran
     SUBROUTINE test(array,n,ml,m2)
     INTEGER,INTENT(IN)::n,ml,m2
     REAL,DIMENSION(n,ml,m2)::array
     ```

   - 不定结构形参数组

     不定结构形参数组只能用在**带有显式接口的过程**中。

     ```fortran
     SUBROUTINE tst(array)
     REAL,DIMENSION(:,:): :array
     ```

   - 不定大小的形参数组

     在新的程序中不应该再去使用它。

     ```fortran
     SUBROUTINE test(array)
     REAL,DIMENSION(l0,*) ::array
     ```

3. **自动数组**

   自动数组是出现在过程中的显式结构数组， 它没有恒定的下标值。

   ```fortran
   SUBROUTINE test(n,m)
   INTEGER,INTENT(IN)::n,m
   REAL,DIMENSION(n,m)::array !Bounds in argument list,but not array
   ```

4. **预定义结构数组**

   预定义结构数组是一个可分配数组或者指针数组。利用ALLOCATABLE属性（或POINTER属性）在类型声明语句中声明。

   ```fortran
   INTEGER,ALLOCATABLE::array(:,:)
   ALLOCATE(array(l000,1000),STATUS=istat)
   ...
   DEALLOCATE(array,STATUS=istat)
   ```

## 8.5 在过程中作为形参的可分配数组

### 8.5.1 可分配形式参数

如果子例程有显式接口(module)， 那么对于子例程(subroutine)来说， 它的**形式参数可以是可分配的**。如果声明形式参数为可分配的，那么用于调用子例程的相应实际参数也**必须是可分配的**。

示例：

```fortran
MODULE test_module
    CONTAINS
        SUBROUTINE test_alloc(array)
            IMPLICIT NONE
            !测试数组
            REAL,DIMENSION(:),ALLOCATABLE,INTENT(INOUT)::array
            !局部变量
            INTEGER::i
            INTEGER::istat
            !获取数组状态
            IF(ALLOCATED(array))THEN
                WRITE (*,'(A)')'Sub:the array is allocated'
                WRITE (*,'(A, 6F4.1)')'Sub: Array on entry = ',array
            ELSE
                WRITE (*,*)'Sub:the array is not allocated'
            END IF
            !回收数组
            IF(ALLOCATED(array))THEN
                DEALLOCATE(array,STAT=istat)
            END IF
            !按5个元素的矢量再分配空间
            ALLOCATE(array(5),STAT=istat)
            !保存数据
            !DO i=1,5
            !    array(i)=6-i
            !END DO
            array=[(6-i,i=1,5)]
            !退出时显示数组 array 的内容
            WRITE(*,'(A,6F4.1)')'Sub: Array on exit = ',array
        END SUBROUTINE test_alloc
END MODULE test_module

!目的
!说明子例程中可分配数组的使用
PROGRAM test_allocatable_arguments
USE test_module
IMPLICIT NONE
!声明局部变量
REAL,ALLOCATABLE,DIMENSION(:)::a
INTEGER::istat
INTEGER::j
!初始的可分配数组
ALLOCATE(a(6),STAT=istat)
!初始化可分配数组
a=[(1.*j,j=1,6)]
!在调用前显示a数组
WRITE(*,'(A,6F4.1)') 'Main:Array a before call= ',a
!调用子例程
CALL test_alloc(a)
!在调用后显示a数组
WRITE(*,'(A,6F4.1)') 'Main:Array a after call = ',a
END PROGRAM test_allocatable_arguments

```

如果注释掉第43和第45行代码，那么输出：Sub: the array is not allocated。

子例程中，array为可分配数组，因此在测试程序(PROGRAM)中，调用子例程的相应实际参数(a)也必须是可分配的数组。

### 8.5.2 可分配函数

Fortran函数的返回值允许有ALLOCATABLE属性。在函数的入口不会分配返回变量。

```fortran
MODULE test_module
    !说明可分配函数的返回值的使用
    !
    CONTAINS
        FUNCTION test_alloc_fun(n)
            IMPLICIT NONE
            INTEGER,INTENT(IN)::n              !return number of elements
            REAL,ALLOCATABLE,DIMENSION(:)::test_alloc_fun
            !局部变量
            INTEGER::i
            INTEGER::istat
            !获得新数组
            IF (ALLOCATED(test_alloc_fun))THEN
                WRITE (*,'(A)')'Array is allocated'
            ELSE
                WRITE (*,'(A)')'Array is NOT allocated'
            END IF
            !元素的矢址来分配
            ALLOCATE(test_alloc_fun(n),STAT=istat )
            !初始化数据
            !DO i=l,n
            !    test_alloc_fun(i)=6-i
            !END DO
            test_alloc_fun=[(6-i,i=1,n)]
            !退出时显示数组a的内容
            WRITE(*,'(A,20F4.1)')'Array on exit = ',test_alloc_fun
        END FUNCTION test_alloc_fun
END MODULE test_module

PROGRAM test_allocatable_function
!目的：说明可分配函数返回值的使用
USE test_module
IMPLICIT NONE
!明局部变量
INTEGER::n=5
REAL,DIMENSION(:),ALLOCATABLE::res
!调用函数，显示结果
res=test_alloc_fun(n)
!待分配的元素个数
WRITE (*,'(A,20F4.1)')'Function return =',res
END PROGRAM test_allocatable_function
```

## 8.6 纯过程和逐元过程

### 8.6.1 纯过程(PURE)

纯函数：没有任何负面影响的函数。 它们**不会修改输入参数**，也**不会修改任何在函数外部可见的其他数据**（比如模块中的数据）。另外，**局部变量不可以有SAVE属性**，**不可以在类型声明语句中初始化局部变量**（因为这一初始化隐含有SAVE属性）。任何被纯函数调用的过程也必须是纯过程。

纯函数中每个参数都必须定义为INTENT (IN)，被纯函数调用的任何子例程或函数也必须是纯的。

除此之外， 该函数也**不能有任何外部文件I/O操作**， **不能包含STOP语句**。这些限制很容易遵守， 到现在为止创建的所有函数都是纯函数。

在函数语句中增加一个PURE 前缀就可以定义纯函数：

```fortran
PURE FUNCTION length(x,y)
	IMPLICIT NONE
	REAL,INTENT(IN)::x,y
	REAL::length
	length=SQRT(x**2+y**2)
END FUNCTION length
```

纯子例程是没有任何负面影响的子例程。

除了允许它们修改用INTENT (OUT)或者INTENT(INOUT)声明的参数外， 它们的限制和纯函数是相同的。在SUBROUTINE语句中**增加PURE前缀可以声明纯子例程**。

### 8.6.2 逐元过程(ELEMENTAL)

逐元函数是**为标量参数指定的函数**。它也适用于数组参数。如果一个逐元函数的参数是标量，那么这个函数的返回值也是标量。如果函数的参数是数组，那么函数的返回值也是和输入参数相同结构的数组。

**用户自定义的逐元函数一定是PURE函数**， 必须满足下面额外的限制：

1. 所有的形式参数都必须是标堡，不能带有指针(POINTER) 属性。

2. 函数的返回值也必须是标量，不能带有POINTER属性。

3.  **除非作为某种内置函数的参数**，**形式参数不能用在类型声明语句中**。这种限制**阻止了自动数组在逐元函数中的使用**。

   在函数语句中增加一个**ELEMENTAL**前缀可以声明用户自定义逐元函数。函数sinc(x) 是逐元函数， 它将会被做如下声明：

   ```fortran
   ELEMENTAL FUNCTION sinc(x)
   ```

   如果sinc函数声明为ELEMENTAL ,那么该函数也**可以接收数组参数**，并返回数组结构值。

   逐元子例程是为标量参数指定的子例程， 也适用于数组参数。它和逐元函数具有同样的限制。在子例程语句中增加一个ELEMENTAL前缀可声明逐元子例程。例如：

   ```fortran
   ELEMENTAL SUBROUTINE convert(x,y,z)
   ```

示例：计算矩形面积：

```fortran
ELEMENT FUNCTION rec_area_e(x,y)
	IMPLICIT NONE
	REAL,INTENT(IN):x,y
	REAL::rec_area_e
	rec_area_e=x*y
END FUNCTION rea_area_e

PROGRAM func_test
	IMPLICIT NONE
	REAL::rec_area_e
	REAL::x1,y1
	REAL,DIMENSION(3),x2,y2
	x1=1.
	y1=2.
	x2=[1,2,3]
	y2=[4,5,6]
	WRITE(*,100)rec_area_e(x1,y1)
	100 FORMAT('单个标量型：矩形面积为：',F5.4)
	WRITE(*,100)rec_area_e(x2,y2)
	100 FORMAT('数组型：矩形面积为：',3F5.4)
```

如此运行会报错：

```fortran
Error: Explicit interface required for 'rec_area_e'  : elemental procedure|
```

去掉ELEMENT属性即不会报错。

因此：**如果要使用逐元函数，需要写出显式接口(MODULE)。**

```fortran
MODULE Display_interface
    CONTAINS
        ELEMENTAL REAL FUNCTION rec_area_e(x,y)
            IMPLICIT NONE
            REAL,INTENT(IN)::x,y
            rec_area_e=x*y
        END FUNCTION rec_area_e
END MODULE Display_interface

PROGRAM func_test
    USE Display_interface
	IMPLICIT NONE
	REAL::x1,y1
	REAL,DIMENSION(3)::x2,y2
	x1=1.
	y1=2.
	x2=[1.,2.,3.]
	y2=[4.,5.,6.]
	WRITE(*,100)rec_area_e(x1,y1)
	100 FORMAT('单个标量型：矩形面积为：',F10.4)
	WRITE(*,110)rec_area_e(x2,y2)
	110 FORMAT('数组型：矩形面积为：',3F10.4)
END PROGRAM func_test

```

输出：

```fortran
单个标量型：矩形面积为：    2.0000
数组型：矩形面积为：    4.0000   10.0000   18.0000
Process returned 0 (0x0)   execution time : 0.047 s
Press any key to continue.
```

### 8.6.3不纯逐元过程(IMPURE ELEMENTAL)

逐元过程也可以被设计成**可以修改其调用函数**，如果这样，此时该过程称为不纯逐元过程。

这类过程必须使用IMPURE关键词声明，并且**被修改的参数必须使用INTENT(INOUT)声明**。

当在数组中调用不纯逐元过程时，该过程按照数组顺序a (1), a (2), a (3), …,a(n)逐步执行。如果是多维数组， 元素将以列：a(1,1), a (2,1), …等为主顺序执行。

示例：不纯逐元过程cum。此函数将数组中的每个值替换为数组中该点及之前点的所有值的总和。

```fortran
MODULE Display_interface
    CONTAINS
        IMPURE ELEMENTAL REAL FUNCTION cum(a,sum)
            IMPLICIT NONE
            REAL,INTENT(IN)::a
            REAL,INTENT(INOUT)::sum
            sum=sum+a
            cum=sum
        END FUNCTION cum
END MODULE Display_interface

PROGRAM test_cum
    USE Display_interface
    IMPLICIT NONE
    REAL,DIMENSION(5)::a,b
    REAL::sum
    sum=0.
    a=[1.,2.,3.,4.,5.]
    b=cum(a,sum)
    WRITE(*,*)b
END PROGRAM test_cum

```

输出：

```fortran
1.00000000       3.00000000       6.00000000       10.0000000       15.0000000
Process returned 0 (0x0)   execution time : 0.107 s
Press any key to continue.
```

为理解程序的运行逻辑，我们可以进行如下分析：

首先，函数修改了输入参数sum，因此不是纯函数。上述过程如下

```
sum=0;a=[1,2,3,4,5]
cum(a(1),sum):
sum=a(1)+sum=1
cum(1)=sum=1
cum(a(2),sum):
sum=a(2)+sum=3
cum(2)=sum=3
...
```

对于数组来说，该函数类似在执行循环，一般可用于数组各项元素之间有联系的操作。

## 8.7 内部过程(CONTAINS)

- 定义：不同于**外部过程**和**模块过程**。内部过程是完全包含在另一个被称为**宿主程序单元**或者就叫宿主(host) 的程序单元中的过程。内部过程和宿主一起编译，**只能从宿主程序单元中调用它**。像模块过程一样，内部过程用**CONTAINS**语句来引入。内部过程**必须跟在宿主过程的所有执行语句之后**， 而且**必须用CONTAINS语句引入**。
- 作用：在某些问题中，作为解决方案的一部分，**有一些低级操作可能要重复执行**。经定义内部过程来完成这些操作，可以简化这些低级操作。

示例：调用内部过程

```fortran
PROGRAM test_interal
!说明内部过程的使用
IMPLICIT NONE
REAL,PARAMETER::PI=3.1415926
REAL::theta
WRITE(*,*)'请输入期望的角度值：'
READ(*,*)theta
WRITE(*,'(A,F10.4!)')'正切值为：'secant(theta)

!内部过程 放在最后
CONTAINS
	REAL FUNCTION secant(angle_in_degrees)
		REAL:: angle_in_degrees
		secant=1./cos(angle_in_degrees*pi/180.)
	END FUNCTION secant
!
END PROGRAM test_internal
```

内部过程函数和外部过程有以下三方面的区别：

1. 内部过程**只能被宿主过程调用**，程序中的**其他过程不能访问它**。
2. **内部过程的名字不能作为命令行参数传递给其他的过程**。
3. 内部过程通过**宿主**关联继承了宿主程序单元的所有数据实体（参数和变量）。即当在宿主程序单元中定义内部过程时，**宿主程序单元中的所有参数和变量都可以在内部过程中使用**。(无需IMPLICIT NONE 语句，因为宿主程序中的变量都可以用于内部过程，除非内部过程定义变量与宿主程序同名，则不能访问宿主数据，但内部过程中发生的所有操作不会对宿主程序中的数据造成任何影响。)

## 8.8 子模块

- Fortran的两种显式接口：**模块化**和**interface**

将模块中的过程分为两部分， 第一部分是模块本身， 包含每个模块**过程的接口**（**调用参数**），第二部分是包含过程的**实际可执行代码的子模块**。

如果**任何过程的接口发生更改**，则**必须重新编译使用模块的所有其他过程**。

如果只在子模块中更改过程的实施内容（可执行代码），则只需要重新编译子模块。

子模块过程的接口没有任何更改，因此程序的其他部分不需要修改或重新编。

通过将**接口**包含到**模块的过程**和**子模块的可执行代码中**， 可以将过程放入模块/子模块组合中。注意模块包含一个INTERFACE块，而不是CONTAINS语句，并且**每个过程的接口都是由关键字MODULE引入**的。Fortran编译器自动从接口块中生成一个显式接口。

```fortran
MODULE test_module
	IMPLICIT NONE
	INTERFACE
	!第一部分，模块本身，包含过程的接口
		MODULE SUBROUTINE procedure(a,b,c)
			IMPLICIT NONE
			REAL,INTENT(IN)::a
			REAL,INTENT(IN)::b
			REAL,REAL,INTENT(OUT)::c
		END SUBROUTINE procedure

		MODULE REAL FUNCTION func2(a,b)
			IMPLICIT NONE
			REAL,INTENT(IN)::a
			REAL,INTENT(IN)::b
		END FUNCTION func2
	END INTERFACE
END MODULE test_module
```

然后将可执行代码置入子模块中：

```fortran
!第二部分，可执行代码的子模块
SUBMODULE(test_module)test_module_exec
IMPLICIT NONE
CONTAINS
	MODULE PROCEDURE procedure1
	...
	END PROCEDURE procedurel
	MODULE PROCEDURE func2
	...
	END PROCEDURE func2
END SUBMODULE test_module_exec
```

通过SUBMODULE 语句将该子模块声明为test_module 子模块。注意这里**没有定义每个模块过程的输入和输出参数**，它们**将从模块的接口定义中继承过来**。如代码用此方法编写，**则子模块的内容可以更改和重新编译， 但无需重新编译依赖于它的程序部分。**

示例：

```fortran
!创建模块
MODULE test
    IMPLICIT NONE
    !通过INTERFACE块，将过程的接口引入
        INTERFACE
        !每个过程的接口都是由关键字MODULE引入，定义输入和输出参数
            MODULE REAL FUNCTION func(a,b,c,x)
                IMPLICIT NONE
                REAL,INTENT(IN)::a,b,c
                REAL,INTENT(IN)::x
            END FUNCTION
        END INTERFACE
END MODULE TEST
!创建子模块
SUBMODULE(test)subtest
    IMPLICIT NONE
    !通过contains块，写入可执行代码
    CONTAINS
    !MODULE PROCEDURE ...END PROCEDURE  之间写入可执行代码
    MODULE PROCEDURE func
    func=a*x**2+b*x+c
    END PROCEDURE func
END SUBMODULE subtest
!测试程序
PROGRAM main
    USE test
    IMPLICIT NONE
    REAL::a,b,c,x
    READ(*,*)a,b,c,x
    WRITE(*,*)func(a,b,c,x)

END PROGRAM
```

## 8.9 小结

### 8.9.1 遵循原则：

- 始终应该用**显式结构形参数组**或者**不定结构形参数组**作为形参数组参数。**永远不要在任何新的程序中使用不定大小形参数组。**

- 如果需要在连续的过程调用期间过程中的**变量值不被改变**，那么在变量的类型声明语句中指明SAVE属性，包含该变量在SAVE语句中，或者在它的类型声明语句中初始化该变量。
- 在过程中使用自动数组来创建局部临时工作数组。使用可分配数组创建在主程序中的数组，或者在不同的过程中创建或销毁的数组、或者在给定的过程中能够改变大小的数组。
- 使用**内部过程**完成只需要在一个程序单元中执行，且必须重复执行的**低级操作**。
- 使用**子模块**从过程接口中**分离可执行代码**，使得在**不需要强制主要内容重新编译的情况下，更简单地修改内部代码。**

### 8.9.2 语法小结

1. CONTAINS语句

   ```fortran
   PROGRAM main
   ...
   	CONTAINS
   		SUBROUTINE subl(x,y)
   		...
   		END SUBROUTINE subl
   END PROGRAM
   ```

   CONTAINS语句是指明下面的语句是宿主单元中的一个或多个独立过程的过程。

   当用于**模块**时，CONTAINS 语句标志着一个或多个**模块过程的开始**。

   当用于**主程序**或者**外部过程**时， CONTAINS语句标志着**一个或多个内部过程的开始**。

   CONTAINS语句必须出现在任何类型、接口及模块中的**数据定义之后**， **必须跟在主程序或者外部过程的最后一条执行语句之后。**

2. ELEMENTAL前缀

   ```fortran
   !格式：
   ELEMENTAL FUNCTION name(argl,...)
   ELEMENTAL SUBROUTINE name(argl,...）
   !例子：
   ELEMENTAL FUNCTION my_fun(a,b,c)
   ```

   这个前缀定义一个过程是逐元的，这意味着它是用标量输入和输出定义的。也可以用于数组的输入和输出。当用于数组的时候，逐元过程定义的操作按**逐个元素的方式作用于输入数组中的每个元素上**。

3. END SUBMODULE语句

   ```fortran
   !格式：
   END SUBMODULE[module—name]
   !例子：
   END SUBMODULE solvers exec
   ```

   该语句标记子模块的结束。

4. PURE前缀

   ```fortran
   !格式：
   PURE FUNCTION name (argl,...)
   PURE SUBROUTINE name(argl,…）
   !例子：
   PURE FUNCTION my_fun(a,b,c)
   ```

5. SAVE属性

   ```fortran
   !格式：
   Type,SAVE::namel,name2,…
   !例子：
   REAL,SAVE::sum
   ```

   这个属性声明过程中的**局部变量的值**在**连续的调用过程期间**必须**保待不变**。它和在SAVE语句中包含变量名是等价的。

6. SAVE语句

   ```fortran
   !格式：
   SAVE [varl,var2,... ]
   !例子：
   SAVE count,index
   SAVE
   ```

   这条语句定义一个过程中的局部变量的值在连续的调用过程期间必须保持不变。如果包含一系列变量， 那么只有那些变量会保存，如果没有包含任何变量，那么在过程或者模块中的每个局部变量都会保存下来，以保持不变。

7. SUBMODULE语句

   ```fortran
   !格式：
   SUBMODULE(parent_module)module_name
   !例子：
   SUBMODULE(solvers)solvers_exec
   ```

   该语句用于声明一个子模块， 用于将程序的**可执行代码**从接口（调用参数表）中分离出
   来，子模块在父模块中声明。

   

