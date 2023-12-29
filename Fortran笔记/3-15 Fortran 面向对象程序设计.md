# 15 面向对象程序设计

## 15.1 面向对象程序设计介绍

### 15.1.1 对象

任何一个具体的对象都可以从两个不同的方面来刻画：**属性**和**行为**。每一个对象都由若干数据（称为**属性**）和行为（称为**方法**）组成。属性是一些描述对象基本特征的变量，方法描述对象的行为以及如何修改对象的属性。因此，一个对象便是变量以及相关方法的软式联合体。

对象可以被想象为一个细胞，该细胞核由内核变量（含有对象的属性）和外层方法构成，方法是对象变量与外部世界的接口，使得数据内核通过外层方法对外部世界隐藏起来。这被称为对象的变量封装在对象中，意味着对象外部的代码不可能看见或直接操作内部变量。任何对变量数据的访问都必须通过调用方法来实现。

对象中的变量和方法也被称为实例变量和实例方法。每一个指定类型的对象都会复制自己的实例变量，但是所有的对象共享同样的实例方法。

通常，如果程序中的其他对象不能看到一个对象的内部状态，它们也就不会因偶然的修改对象状态而给程序引入错误。此外，对对象内部操作的修改不会影响到程序中其他对象的操作。只要对象跟外部的接口没有被改变，在任何时候修改对象的实现细节， 都不会影响程序的其他部分。

封装提供了两点主要的好处：

1. 模块性。一个对象的编写和维护独立于其他对象源代码。因此，一个对象可以很容易被重用，以及用于系统中的其他部分中。
2. 信息隐藏。每一个对象都具有一个和其他对象交互的公共接口（方法调用列表）。然而，对象的实例化变量是不能被其他对象直接访问的。因此，只要公共接口没有改变，对象的变量和方法在任何时候都可以被修改，却不会对依赖于该对象的其他对象造成影响。尽量使变量为私有，以保证它们被隐藏在对象中。这样的封装使程序模块性更好， 并易于修改。

### 15.1.2 消息

在面向对象编程模型中，对象之间通过来回地发送消息进行通信。这些消息实际上就是方法调用。

每条消息都包含三个部分，为接受消息的对象完成所要求的动作提供必需的信息。

1. 一个要访问消息的对象的**引用**。
2. 对象要执行的**方法名**。
3. 方法需要的**全部参数**。

### 15.1.3 类

在面向对象编程中，类是**创建对象的蓝本**。

类是一个软件结构，它**指定了对象中包含的变量类型和个数**，以及对象中定义的方法。

类中的每一个组件被称为成员。

有两种类型的成员，一种是**数据域**（数据成员），它指定了类中定义的数据类型，另一种是**方法**（成员函数），用来定义对数据域的操作。

### 15.1.4 类的层次结构和继承

面向对象语言中的类采用类层次结构来组织，位于最高层的基类定义了通用的行为， 而较低层的类更具特殊性。

每一个低层的类都基于更高层次的类，或者从较高层的类派生而来。派生的低层类从上层继承了实例变量和实例方法。

新类一开始就具有基类的所有实例变量和方法，然后程序员可以根据功能需要在新类中添加新的变量和方法。

新类基于的那个类称为**父类**或**超类**，新类称为**子类**。新的子类也可以作为另一个子类的超类。子类通常会添加自己的实例变量和实例方法，因此，子类通常会更大千它的超类。

### 15.1.5 面向对象编程

面向对象编程(Object-Oriented Programming OOP)在软件中模块化对象的编程过程。在OOP中，程序员分析要解决的问题，并将它们分解为特定的对象，每一个对象都包含一定的数据和操作这些数据的特定方法。

有时，这些对象和客观世界中的对象相对应，而有时候它们纯粹是抽象的软件结构。一旦确定了问题分解的对象，程序员要做的就是确定每个对象中作为实例变揽存放的数据类型，以及用来操作这些数据需要的方法的精确调用参数序列。

接下来，程序员可以一次性开发并测试模块中的类。只要不改变类间的接口（方法调用参数序列），每一个类都可单独开发和测试，而不需要改变程序的其他部分。

## 15.2 Fortran 类的结构

一个Fortran 类的主要组件（类成员）包括以下部分：

1. 数据域（数据成员，field)。当从类实例化对象时，将**创建实例化的变量称为数据域**，实例化变量是封装在对象中的数据，每次对象从类实例化时，都会创建新的实例变量集。
2. 方法（成员函数，Method)。方法**实现类的行为**。有些方法可能在类中有明确定义，但有些方法可能从超类中继承而来。
3. 构造函数(Constructor)。当对象被创建后，构造函数用来**初始化对象中的变量**。Fortran对象既可以用结构构造函数初始化，也可以使用特殊的初始化方法初始化。
4. 析构函数(Finalizer)。在一个对象被销毁前，它将调用一个特殊的方法——析构函数。此方法**完成对象销毁前所有必祔的清除工作（释放资源等）**。**类中最多有一个析构函数，也有很多类根本不需要析构函数**。

当一个对象想要访问类中的成员时，无论是变量还是方法，都要通过使用组件选择器来完成。组件选择器用％标识符表示。例如，假设有一个包含**变量a**和**方法process_a()**的**类my_class**, 如果该类生成的对象名为my_obj, 那么用my_obj¾a来访问my_obj中的实例变量，my_obj%process_a(）来访问其方法。

## 15.3 CLASS 保留字

CLASS 保留字是TYPE保留字的变形。TYPE保留字用来添加特殊属性，对面向对象编程很重要。

在常规的Fortran程序中，过程中形参的类型和调用时相应的实参应该完全匹配，否则将会引发错误。类似地，指针类型和它所指向的类型也必须匹配，不然的话，也会有错误发生。

可分配变量和相应的数据也必须是匹配的，否则会出错。CLASS保留字以一种特殊的方式放宽了这个要求。

如果一个**可分配数据项**、**指针**，或者**形参**用CLASS (type)保留字声明，这里type是一个派生数据类型，那么**数据项将与数据类型或数据类型的所有扩展相匹配**。

示例：

```fortran
TYPE point    
    REAL::x
    REAL::y
END TYPE
TYPE,EXTENDS(point)::point_3d
    REAL::z
END TYPE
TYPE(point),POINTER::p_1
CLASS(point),POINTER::p_2
```

对于p_1它只能接受point类型的数据，而对于p_2无论是point类型还是point_3d这样的point扩展类型的数据都可以被接受。

以CLASS保留字声明的指针或者形参类型，称为**指针或者形参的声明类型**。而任何时候分配给指针或者形参的实际对象的类型被称为指针或形参的**动态类型**。

因为用CLASS保留字声明的数据项可以和一种以上的数据类型相匹配，所以被认为是**多态的**（意思是“多种形式”)。

多态指针或形参有一个特殊的限制： **仅能用它们来访问声明类型的数据项。在扩展中定义的数据项不能用多态指针访问。**

示例：

```fortran
CLASS(point),POINTER :: p
TYPE(point),TARGET :: p1
TYPE(point_3d),TARGET :: p2
!说明
P=>p1
!这两行产生同样的输出
WRITE (*,*) p1%x, p1%y   
WRITE (*,*) p%x, p%y   
p => p2
WRITE (*,*) p2%x, p2%y, p2%z
WRITE (*,*) p%x, p%y, p%z
!错误，指针p不能访问p%z
```

在这些定义中，变量p1和p2都能赋值给p, 而且指针p能用来访问pl和p2中的x和y成员组件。但是，指针p不能用来访问z组件，原因是**z组件没有在指针的声明类型中定义**。

可以通过**SELECT_TYPE**结构来绕过这个限制。

用CLASS(＊)定义指针或者形参称为不受限多态性， 它可以**与任何派生类型相匹配**。然而，不能直接访问动态数据类型的任何成员组件，因为在指针或者形参的声明类型中没有定义任何组件。

## 15.4 在 Fortran 中实现类和对象

每一个Fortran类都应该放在一个独立的模块中，以便可以控制访问它的成员组件，且通过USE访问显式的类接口。

### 16.4.1 声明数据域（实例对象）

类中的数据域定义在用户定义的数据类型中，而且数据类型的名字是类的名字。

在严格的面向对象编程中，**数据类型**应当用**PUBLIC**来声明，而数据类型的**成员组件**则用**PRIVATE**声明。

这样做，使得在模块外创建该类型的对象成为可能，但是从**模块外**读取或者修改该类型的实例变量是不可能的。
在**实际的面向对象Fortran程序中，经常不把数据类型的成员组件声明为PRIVATE**。如果一个Fortran对象具有继承了超类数据的子类，那么那个数据必须声明为PUBLIC，否则（那些定义在不同模块中的）该子类将不能访问这个数据。此外，**如果数据域被声明为PRIVATE,那么Fortran语言不允许构造函数使用它们。**这是Fortran对于面向对象编程实现的限制。

示例：定义一个简单的复数

```fortran
MODULE complex_class
IMPLICIT NONE
!类型定义
TYPE,PUBLIC :: complex_ob
PRIVATE
REAL :: re
REAL :: im
END TYPE complex_ob
!这添加方法
CONTAINS
!（插入方法代码在此）
END MODULE complex_class
```

如果类中的**数据域**用PUBLIC声明，那么这个类的构造函数可以用来初始化实例变量。
构造函数由数据类型名组成，其后的圆括号中是数据元素的初始值。

例如，如果类中的数据域被声明为PUBLIC，那么下面的代码将创建一个复数对象，对象中初始的x和y值为1和2,并且将这个对象赋值给指针p。

```fortran
CLASS(complex_ob), POINTER::p
TYPE(complex_ob),TARGET::a=complex_ob(1.,2.)
p=>a
```

**如果类中的数据域声明为PRIVATE, 那么程序员必须编写一个特别的方法来初始化类中的数据。**

### 16.4.2 创建方法

面向对象方法不同于普通的Fortran 过程，它们绑定于一个特定的类，而且仅能作用于类中的数据。可以通过在类型定义中添加CONTAINS语句来创建，而且在语句后声明绑定。

示例：

```fortran
MODULE complex_class
IMPLICIT NONE
!类型定义
TYPE,PUBLIC :: complex_ob
!PRIVATE
!这将是实例化名
REAL :: re !实数部分
REAL :: im !虚数部分
CONTAINS
!绑定过程
PROCEDURE,NOPASS:: set_value =>complex_a_b
PROCEDURE::add => add_complex_to_complex
END TYPE complex_ob
TYPE,EXTENDS(complex_ob)::complex_ob3
    REAL::new
    CONTAINS
    PROCEDURE,NOPASS::set_values => complex_a_b_c
END TYPE

!INTERFACE set_value
!    MODULE PROCEDURE complex_a_b
!    MODULE PROCEDURE complex_a_b_c
!END INTERFACE
!声明对模块的访问
PRIVATE :: add_complex_to_complex
PRIVATE :: complex_a_b
PRIVATE :: complex_a_b_c
CONTAINS
!插入赋值方法
    FUNCTION complex_a_b(a,b) RESULT(c)
        TYPE(complex_ob)::c
        REAL::a,b
        c%re=a
        c%im=b
    END FUNCTION
    FUNCTION complex_a_b_c(a,b,c) RESULT(d)
        TYPE(complex_ob3)::d
        REAL::a,b,c
        d%re=a
        d%im=b
        d%new=c
    END FUNCTION
!插入方法add_complex_to_complex:
SUBROUTINE add_complex_to_complex(a,b,c)
    CLASS(complex_ob):: a,b,c
    c%re=a%re+b%re
    c%im=a%im+b%im
END SUBROUTINE add_complex_to_complex
END MODULE  complex_class

PROGRAM test
    USE complex_class
    IMPLICIT NONE
    TYPE(complex_ob)::a,b
    TYPE(complex_ob3)::c
    a=a%set_value (1.,2.)
    b=b%set_value (1.,2.)
    c=c%set_values(1.,2.,3.)
    WRITE(*,*)a,b,c
END PROGRAM
```

### 16.4.3 由类创建（实例化）对象

通过在过程中用USE语句来使用complex_class 模块，使得complex_ob类的对象可以在另一个过程中被实例化，然后用TYPE保留字声明该对象。

```fortran
USE complex_class
IMPLICIT NONE
TYPE (complex- ob) ::x,y,z
```

## 15.5 第一个例子 timer 类

计时器是非常好的第一个对象，因为它很简单。它类似于一个秒表。秒表用来计算从按下开始按键到按下结束按键（通常和开始按键是同一个按钮）之间所经过的时间。

秒表所完成的基本动作（即方法）包括如下儿项：

1. 按下按键， 重置开始计时
2. 按下按键， 结束计时， 并显示计时时长。

一个计时器类需要包括以下的组件（成员）：

1. 用来保存起始时间的方法(**start_timer**)。调用这个方法时不需要任何输入参数，而且它也没有返回值。
2. 返回从最后一次计时开始绊起所经过的时间的方法(**elapsed_time**)。这个方法也不需要任何输入参数，但是它将返回给调用程序经过的时间的长度，以秒为单位。
3. 使用计时方法时要有保存计时器**开始计时的时间点的数据域**（**实例变量**）。

```fortran
!timer程序
MODULE timer_class
    IMPLICIT NONE
    INTEGER,DIMENSION(8)::value
    !声明常量
    INTEGER,PARAMETER::DBL=SELECTED_REAL_KIND(p=14)
    !定义类型
    TYPE,PUBLIC:: timer
        PRIVATE
        REAL(KIND=DBL)::saved_time
        !绑定过程
        CONTAINS
            PROCEDURE,PUBLIC::start_timer=>start_timer_sub
            PROCEDURE,PUBLIC::elapsed_timer=>elapsed_time_fn
    END TYPE
    PRIVATE::start_timer_sub
    PRIVATE::elapsed_time_fn
    !写入函数和子例程
    CONTAINS
        !开始计时程序(存入时间，无返回值)
        SUBROUTINE start_timer_sub(time)
            IMPLICIT NONE
            CLASS(timer),INTENT(INOUT)::time
            CALL DATE_AND_TIME(VALUES=value)
            time%saved_time= 86400.D0*value(3) + 3600.D0*value(5)+&
            				60.D0 *value(6) + value(7) + 0.001D0 * value(8)
        END SUBROUTINE
        !结束计时程序(返回运行时间)
        FUNCTION elapsed_time_fn(start_time) RESULT(run_time)
            IMPLICIT NONE
            CLASS(timer),INTENT(INOUT)::start_time
            REAL::run_time
            REAL::elapsed_time
            CALL DATE_AND_TIME(VALUES=value)
            elapsed_time=86400.D0*value(3) + 3600.D0*value(5)+&
            			60.D0 *value(6) + value(7) + 0.001D0 * value(8)
            run_time=elapsed_time-start_time%saved_time
        END FUNCTION
END MODULE
!测试模块
PROGRAM test
USE timer_class
IMPLICIT NONE
INTEGER::i,j,k
TYPE(timer)::time
CALL time%start_timer()
!运行的程序
DO i = 1, 100000
    DO j = 1, 100000
        k = i + j
    END DO
END DO
!结束
WRITE(*,*)'RUN TIME: ',time%elapsed_timer()
END PROGRAM
```

**注意事项：**

1. 首先要注意的是，timer类用实例变量saved_time 来保存开始时间。每一次从类实例化一个对象，它都将获得自己的类中定义的所有实例变量的拷贝。因此，**可以在同一个程序中，同时实例化及使用多个timer对象**，而对象间不会相互影响，因为每一个计时器都拥有属于自己的私用实例变量saved_time的拷贝。
2. 每一个类成员都用关键字PUBLIC或者PRIVATE声明。任何用PUBLIC声明的实例变量或者方法都可以被程序的其余部分用USE关联来访问。而任何用PRIVATE声明的实例变量和方法只能被定义了该变量或方法的对象本身来访问记住应当总是将类中的所以实例变量声明为PRIVATE。
3. 实际命名为start_timer_sub 和elapsed_time_ fn的方法被声明为PRIVATE, 这意味着不能从程序的另外其他部分直接调用它们，唯一执行这些方法的办法是用对象名和成员选择器（％）来实现。

## 15.6 方法的分类

因为实例变量通常隐藏在类中，操作它们的唯一途径是**通过类的方法所构成的接口完成**。

**方法是类的公共接口**，是操作信息的标准方式，对用户隐藏了不必要的方法实现细节。
类的方法必须实现一些通用的类似于“ 管家” 的功能，以及完成类所要求的一些特殊动作。

这些“ 管家” 功能有几大分类， 对于大多数的类来说它们都是通用的，跟类的不同用途无关。

通常来说，类必须提供向实例变量存储数据、读取实例变量、监测实例变量状态的方法，以及为解决问题而必须对变量的操作等功能。

**由于不能直接使用类中的实例变量，就必须在类中定义从实例变量存和取数据的方法。**

按照面向对象程序员的习惯，存储数据的方法名以set 开头，称为设置方法，读取数据的方法以get开头，称为读取方法。

设置方法从外部获得信息，并将它存储在类的实例变量中。在这个过程中，应当检查数据的合法性和一致性，以避免类的实例变量被设置为非法状态。

利用设置方法和私有的实例变量， 可以通过检查输入参数避免这种非法行为的发生。

## 15.7 对类成员的访问

**类中的实例变量通常以PRIVATE 声明，而类中的方法通常声明为PUBLIC**, 因此，方法构成了类和外部世界的接口，向程序其他部分隐藏了类的内部行为的细节。这样做有多种好处，因为它使得程序的模块性更好。

假设编写了一个广泛使用timer对象的程序，如果必要，可以完全重新设计timer类的内部行为，只要不改变start_time()和elapsed_time()方法的参数或返回值，程序就依然能够正常工作。

公用接口将类的内部和程序其他部分隔离开来，使得更易于做进一步的修改。

类中的实例变量通常应声明为PRIVATE, 而类方法应被用于提供类的标准接口。

这个普遍的原则也有例外。许多类中包含若干个PRIVATE方法，支持类中的PUBLIC方法完成某些特殊的计算，称这些方法为实用方法。由于原本就不打算让用户直接调用它们，所以用PRIVATE访问修饰符声明它们。

## 15.8 析构函数

一个对象被销毁前，它将调用一个被称为析构函数的特殊方法，假如这个特殊的方法已经被定义了。

析构函数完成对象销毁前全部必要的清理工作（释放资源，关闭文件等）。类中可以有一个以上的析构函数，但大多数的类也根本不需要析构函数。

通过在类型定义的**CONTAINS**部分附加一个**FINAL**关键字，可以把**析构函数和类绑定**。

当销毁一个该数据类型的数据项时，它销毁前将自动调用析构子例程,其参数为对象名。这个子例程能够释放分配的所有内存空间， 因此就**避免了内存泄漏**。最后，析构子例程也可以用来关闭在对象中可能打开的文件，以及**释放系统资源**。

**示例：使用析构函数**

```fortran
MODULE vector_class
    IMPLICIT NONE
    TYPE,PUBLIC::vector
        PRIVATE
        REAL,DIMENSION(:),POINTER::v
        LOGICAL::v_allocated=.FALSE.
    CONTAINS
        !绑定的过程
        PROCEDURE,PUBLIC::set_vector=>set_vector_sub
        PROCEDURE,PUBLIC::get_vector=>get_vector_sub
        PROCEDURE,PUBLIC::sort_vector=>sort_vector_sub
        FINAL::clean_vector_sub
    END TYPE vector
    !限制对这些子例程的直接访问
    PRIVATE:: set_vector_sub,get_vector_sub,clean_vector_sub

    !类的方法
    CONTAINS
    !设置方法
    !set_vector
        SUBROUTINE set_vector_sub(vec,array)
            IMPLICIT NONE
            !声明调用参数
            CLASS(vector)::vec
            REAL,DIMENSION(:),INTENT(IN)::array
            INTEGER::istat
            !如果vec已经被分配了，那么释放内存重新分配
            IF(vec%v_allocated)THEN
                DEALLOCATE(vec%v,STAT=istat)
            END IF
            !这里有一个size函数，size(array,j),指的是
            !array数组第j个指标下的数目
            !size(array)指数组总元素个数
            ALLOCATE(vec%v(SIZE(array,1)),STAT=istat)
            vec%v=array
            vec%v_allocated=.TRUE.
        END SUBROUTINE set_vector_sub
    !设置方法
    !get_vector
        SUBROUTINE get_vector_sub(vec,array)
            IMPLICIT NONE
            !声明调用参数
            CLASS(vector)::vec
            REAL,DIMENSION(:)::array
            INTEGER::istat
            !判断数组长度
            INTEGER::array_length
            INTEGER::vec_length
            IF(vec%v_allocated)THEN
                array_length=SIZE(array,1)
                vec_length=SIZE(vec%v,1)
                IF(array_length>vec_length)THEN
                    array(1:vec_length)=vec%v
                    array(vec_length+1:array_length)=0
                ELSEIF(array_length==vec_length) THEN
                    array=vec%v
                ELSE
                    array=vec%v(1:array_length)
                END IF
            ELSE
                array=0
            END IF
        END SUBROUTINE get_vector_sub
    !设置方法
    !sort_vector
        SUBROUTINE sort_vector_sub(vec)
            IMPLICIT NONE
            CLASS(vector)::vec
            INTEGER::istat

        END SUBROUTINE sort_vector_sub
    !设置方法
    !clean_vector
        SUBROUTINE clean_vector_sub(vec)
            IMPLICIT NONE
            TYPE(vector)::vec
            INTEGER::istat
            WRITE(*,*)'IN finalizer...'
            IF(vec%v_allocated)THEN
                DEALLOCATE(vec%v,STAT=istat)
            END IF
        END SUBROUTINE clean_vector_sub
END MODULE vector_class

PROGRAM test
    !调用模块
    USE vector_class
    !声明变量
    REAL,DIMENSION(6)::array
    REAL,DIMENSION(4)::array1
    INTEGER::istat
    TYPE(vector),POINTER::my_vec

    !用指针创建对象
    ALLOCATE(my_vec,STAT=istat)
    array=[(i,i=1,6)]
    WRITE(*,*)array
    CALL my_vec%set_vector(array)
    !从该vector回收数据
    array=0
    CALL my_vec%get_vector(array1)
    WRITE(*,*)array1
    !销毁对象
    DEALLOCATE(my_vec,STAT=istat)
END PROGRAM
```

## 15.9 继承性和多态性

继承性是面向对象编程的主要优势之一，一旦定义了超类的行为（方法），这个行为就会自动地被它的所有子类继承，**除非显式地重载了这一方法**。因此，只须编写一次代码，就会应用于所有的子类。子类仅需提供方法来实现它本身与它的父类之间的不同操作。

### 15.9.1 超类和子类

- 两个子类对象可以当作单个超类对象集来组合和操作。
- 子类继承了父类中的所有的**PUBLIC实例变量和方法**。

- 如果一个对象想要操作父类中定义的实例变量或重载方法，那么那些实例变量或方法都必须已经用PUBLIC声明。

### 15.9.2 定义和使用子类

通过在类型定义中包含一个EXTENDS属性标识符可以声明某类为另一个类的子类。

```fortran
TYPE,PUBLIC,EXTENDS(class)::sub_class
```

重载方法：

在子类中，如果我们要对父类中的某些方法进行改写，那么我们可以在创建子类的同时，重新绑定同名的方法。

```fortran
MODULE salaried_employee_class
!调用父类
USE employee_class
IMPLICIT NONE
!声明子类
TYPE,PUBLIC,EXTENDS(employee):: salaried_employee
CONTAINS
!绑定的过程
!新增的方法
PROCEDURE, PUBLIC :: set_salary => set_salary_sub
!重载cal_pay方法
PROCEDURE, PUBLIC :: cal_pay => calcyay_fn
...
```

### 15.9.3 超类对象和子类对象间的关系

子类对象继承超类对象的所有变量和方法。实际上，任何一个子类的对象都是一个它的超类的对象。这就意味着既可以通过指向子类的指针，可以通过指向超类的指针来操作该对象。

- 用CLASS保留字声明的超类的指针可以指向子类的对象，因为超类的变量和方法都可以在子类中找到，但子类新声明的变量和方法并不可以进行调用。

- 用CLASS保留字声明的子类的指针不可以指向超类的对象，是非法的会引发编译错误。

### 15.9.4 多态性

多态性是面向对象编程语言所具有的一项令人难以置信的强大特性，它使改变很容易发生。

例如，假设编写了一个使用employee数组来输出工资单的程序，而接下来公司希望增加一种新的按件计薪的雇员类型。

那么， 需要定义一个employee类的新子类piecework_employee,并且相应地重载calc_pay方法，以及创建该类型的雇员对象。

程序的其余部分不需要修改，因为程序中操作的是employee类对象，多态性使Fortran程序能够自动地按照不同对象属于
的子类来选择恰当的方法来执行。

多态性使得不同子类的多个对象能够被当作同一个超类的对象来处理，多态性保证能够根据某一个特定对象所属的子类来选择相应版本的方法。

**需要注意的是，要想使多态性能够发挥作用，就<u>必须在超类中定义方法</u>，并且在各个子类中重载该方法。如果仅仅是在子类中定义方法， 那么多态性是不会发挥作用的。**

原因在于例如用CLASS保留字声明的超类的指针可以指向子类的对象时，如果超类没有定义该方法，即使子类中写入该方法依然不能调用。

### 15.9.5 SELECT TYPE 结构

在**使用超类指针引用对象时**，清楚地分辨出该对象属于哪一个子类是可能的，这是通过使用**SELECT TYPE**结构来做到的。一旦得知了这一信息，程序就可以访问子类独有的变量和方法。

格式：

```fortran
[name:] SELECT TYPE(obj)
TYPE IS (type_1) [name]
Block 1
TYPE IS (type_2) [name]
Block 2
CLASS IS (type_3) [name]
Block 3
CLASS DEFAULT [name]
Block 4
END SELECT [name]
```

**obj**的声明类型是结构中其他类型的超类，但其指向的对象是其本身或是其子类。如果输入对象**obj**的动态类型为**type_1**, 那么将执行块1 (**BLOCK 1**) 中的语句，以此类推。 而且在块语句执行期间，对象指针被认为是**type_1**类型。这就意味着程序可以访问**type_1**子类独有的变量和方法，尽管声明类型**obj**为超类类型。

如果输入对象obj 的动态类型与任何一个**TYPE IS** 子句**均不匹配**，那么结构体将转到**CLASS IS** 处，并执行该块中与输入对象的动态类型**最为匹配的代码**。**执行期间，认为对象类型为这块中声明的类型**。

结构中最多有一个块中的语句被执行。选择执行哪块的原则如下

1. 如果与某个TYPE IS 块匹配，那么执行该块。
2. 否则，如果与某一个CLASS IS块匹配，执行该块。
3. 否则，如果与多个CLASS IS块匹配，则**其中有一个块必定是其他块的扩展**，执行该扩展块。（因为这里面只要是父类匹配一定是有一条线的继承关系的，每个子类的爸爸只有一个，爸爸也只有一个爷爷，取最小的辈分）
4. 否则，如果定义了CLASS DEFAULT块，就执行它。

示例：

```fortran
PROGRAM test
    IMPLICIT NONE
    TYPE::point
        REAL::x
    END TYPE
    TYPE,EXTENDS(point)::point_y
        REAL::y
    END TYPE
    TYPE,EXTENDS(point_y)::point_3d
        REAL::z
    END TYPE
    TYPE,EXTENDS(point_3d)::point_4d
        REAL::t
    END TYPE
    CLASS(point),POINTER::p
    CLASS(point_y),POINTER::p_y
    TYPE(point_3d),TARGET::p_3d
    TYPE(point_4d),TARGET::p_4d
    p_y=>p_4d
    p=>p_y
    SELECT TYPE(p)
    CLASS IS(point_y)
        WRITE(*,*)'point_y'
    CLASS IS(point_3d)
        WRITE(*,*)'point_3d'
    END SELECT
END PROGRAM
!output
!point_3d
```



## 15.10 禁止在子类中重载方法

有时候需要保证一个或多个方法在某个给定超类的子类中不被修改， 这可以在绑定时用NON_OVERRIDABLE属性标识符声明它们来做到这一点：

```fortran
TYPE:: point
REAL: :x
REAL: :y
CONTAINS
PROCEDURE,NON_OVERRIDABLE::myyroc
...
END TYPE
!在point类的定义中使用NON_OVERRIDABLE属性声明了myproe过程，则这个过程就不能被point类的任何一个子类所改写。
```

## 15.11 抽象类

### 15.11.1 抽象类的定义

为了实现多态性，多态方法必须与父类绑定，这样才能够被所有的子类继承。因此，父类中的一些方法往往并不会给父类使用，而是总被子类中相应的方法进行重载并使用。事实是，如果没有对象从父类实例化，那么父类中的方法永远不会被使用。

因此，在Fortran中允许只**声明绑定**和**接口的定义**，而并不需要编写具体的方法。这样的方法被称为**抽象方法**或**不能引用的方法**。包含抽象方法的类型称为抽象类型，以区别于一般的具体类型。

在类型定义中用**DEFERRED**属性来**声明抽象方法**，以及用**ABSTRACT INTERFACE** （抽象接口）来定义方法的调用参数序列。**任何包含了不能引用方法的类型都必须用ABSTRACT属性声明**。从一个抽象类型直接创建对象是非法的，但是创建指向该类型的指针却是合法的。这个指针可以用来操作抽象类型的不同子类对象。

**抽象方法**的格式：

```fortran
PROCEDURE(proc),PUBLIC,DEFERRED::method_name
```

在这条语句中，PROCEDURE后面的括号里填写的是应用于该方法的抽象接口的名字，而method_name是方法的实际名字。

抽象类的所有子类都必须重载超类中所有抽象方法，否则，它们自己也将是抽象的。

抽象类通常位于面向对象编程类层次结构的**顶层**，定义了其所有子类对象行为的主要类型。具体类位于结构层次的更下层， 为每个子类提供具体的实现细节。

### 15.11.2 实现多态性的注意事项

1. **创建一个父类，其中包含了解决问题所需的所有方法**。那些在不同子类中会有变化的方法可以声明为DEFERRED, 如果需要，**都不必在超类中编写方法**，仅仅有接口就可以了。注意这样做将使超类是抽象的(ABSTRACT)，也就意味着不能从它直接实例化对象。
2. **为每一类要操作的对象定义子类。**子类必须为超类中的每一个抽象方法提供特定的实现。
3. **创建不同子类的对象， 井用超类指针引用它们。**当用超类指针调用某方法时，Fortran将自动地执行对象所属子类中的方法。

得到正确多态性的窍门是**决定超类对象应当展示什么样的行为，以及确保每一种行为在超类定义中都有一个方法来代**表。

## 15.12 小结

### 15.12.1 遵循原则

1. 通过保持变量的**私有属性**来将它隐藏在对象之中。这样的封装使得程序具有模块性，并易于修改。
2.  在对实例变量赋值前，使用**set方法**检验输入数据的合法性和一致性。
3. 定义**断言方法**来检查条件的真假，这些条件与所创建的类相关。
4. 类中的实例变量通常应声明为**PRIVATE**, 而类方法被用于提供对类的标准接口。
5. 多态性使得不同子类的多个对象能够被当作单个超类的对象来处理，多态性能够根据某一个特定对象所属的子类来选择相应版本的方法执行。
6. 为了实现多态性，需要**在通用超类中声明所有的多态方法**，然后通过在**每个子类中重载子类继承下来的方法**，改变方法的具体行为。
7.  **使用抽象类型来定义位于面向对象编程类层次结构顶层的行为的主要类型，使用具体类来完成抽象类的子类的实现细节。**

### 15.12.2 语法小结

1. **ABSTRACT 属性**

   ```fortran
   !格式
   TYPE,ABSTRACT :: type_name
   !例子
   TYPE,ABSTRACT :: test
   INTEGER :: a
   INTEGER :: b
   CONTAINS
   PROCEDURE(ADD_PROC),DEFERRED :: add
   END TYPE
   ```

   ABSTRACT属性用来声明**某个数据类型为抽象类型**，这就意味着**不能从该类型创建任何对象**， 其原因在于类中绑定的一个或多个方法不可引用。

2. **ABSTRACT INTERFACE结构**

   ```fortran
   !格式
   ABSTRACT INTERFACE
   ...
   END INTERFACE
   !例子
   TYPE,ABSTRACT :: test
   INTEGER : : a
   INTEGER : : b
   CONTAINS
   PROCEDURE(add_proc),DEFERRED :: add
   END TYPE
   ABSTRACT INTERFACE
   SUBROUTINE add_proc(this, b)
   ...
   END SUBROUTINE add_proc
   END INTERFACE
   ```

   ABSTRACT INTERFACE 结构声明了一个**不能引用过程的接口**，以便Fortran编译器可以知道要求的过程调用参数序列。

3. **CLASS 关键字**

   ```fortran
   !格式
   CLASS(type_name) :: obj，obj2,...
   !例子
   CLASS(point):: my_point
   CLASS (point),POINTER :: pl
   CLASS(*),POINTER :: p2
   
   ```

   CLASS保留字定义了一个指针或形式参数，这个指针或形式参数能够接受某特定类型的目标变量或者该特定类型的任意扩展类型。换句话说，这个指针或形式参数能够操作该特定类型或其任意子类的目标变量。
   CLASS关键字的最终形式创建了一个不受限的多态性指针，该指针可以与全部类的对象匹配，但是对象的数据域和方法仅可以用SELECT TYPE结构来访问。

4. **DEFERRED 属性**

   ```fortran
   !格式
   PROCEDURE, DEFERRED :: proc_name
   !例子
   TYPE,ABSTRACT :: test
   INTEGER :: a
   INTEGER :: b
   CONTAINS
   PROCEDURE(ADD_PROC),DEFERRED :: add
   END TYPE
   ```

   使用DEFERRED属性声明**绑定到某一派生数据类型的过程并没有在该数据类型中定义**，从而使得该类型为**抽象类型**。**该数据类型不能创建任何对象**。在创建该类型的对象前，必须在子类中定义具体实现。

5. **EXTENDS 属性**

   ```fortran
   !格式
   TYPE,EXTENDS(parent_type) :: new_type
   !例子
   TYPE,EXTENDS(point2d)
   REAL :: z
   END TYPE
   ```

   EXTENDS属性指明定义的新类型是EXTENDS属性中指定**类型的扩展**。除了那些在类型定义中已被明确重载的实例变量和方法外，新类型**继承了原类型的所有实例变量和方法**。

6. **NON_OVERRIDABLE属性**

   ```fortran
   !格式
   PROCEDURE,NON_OVERRIDABLE :: proc_name
   !例子
   TYPE :: point
   REAL :: x
   REAL :: y
   CONTAINS
   PROCEDURE,NON_OVERRIDABLE:: my_proc
   END TYPE
   ```

   NON_ OVERRIDABLE属性表明绑定过程不可以被该类的任何派生子类重载。

7. **SELECT TYPE 结构**

   ```fortran
   !格式
   [name:] SELECT TYPE (obj)
   TYPE IS (type_1) [name]
   Block 1
   TYPE IS (type_2) [name]
   Block 2
   CLASS IS (type_3) [name]
   Block 3
   CLASS DEFAULT [name]
   Block 4
   END SELECT [name]
   ```

   SELECT TYPE结构根据obj的特定子类来选择执行的代码段。如果TYPE IS块中的类型精准地与对象类型匹配，则执行该块中的代码，否则如果CLASS IS块的类型是对象超类，则执行那一块。如果多个CLASS IS块是对象的超类，那么执行最高层次超类的代码段。
