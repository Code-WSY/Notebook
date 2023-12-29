# 利用Fortran进行面向对象编程(OOP)的教训1

在Fortran2003的语法种，加入了面向对象编程。

一个Fortran 类的主要组件（类成员）包括以下部分：

1. 数据域。当从类实例化对象时，将**创建实例化的变量称为数据域**，实例化变量是封装在对象中的数据，每次对象从类实例化时，都会创建新的实例变量集。
2. 方法。方法**实现类的行为**。有些方法可能在类中有明确定义，但有些方法可能从超类中继承而来。
3. 构造函数(Constructor)。当对象被创建后，构造函数用来**初始化对象中的变量**。Fortran对象既可以用结构构造函数初始化，也可以使用特殊的初始化方法初始化。
4. 析构函数。在一个对象被销毁前，它将调用一个特殊的方法——析构函数。此方法**完成对象销毁前所有必祔的清除工作（释放资源等）**。**类中最多有一个析构函数，也有很多类根本不需要析构函数**。

在Fortran种实现多态性，我们可以

1. **创建一个父类(抽象类)，其中包含了解决问题所需的所有方法**。那些在不同子类中会有变化的方法可以声明为DEFERRED, 如果需要，**都不必在超类中编写方法**，仅仅有接口就可以了。注意这样做将使超类是抽象的(ABSTRACT)，也就意味着不能从它直接实例化对象。
2. **为每一类要操作的对象定义子类。**子类必须为超类中的每一个抽象方法提供特定的实现。
3. **创建不同子类的对象， 井用超类指针引用它们。**当用超类指针调用某方法时，Fortran将自动地执行对象所属子类中的方法。

但是在实际的操作过程种，笔者发现用Fortran，尤其是利用抽象类进行面向对象编程时，过程种有许许多多的麻烦。

**在抽象类模块中绑定的过程中的形参，在子类只有第一个形参才能声明为子类数据类型，其余必须用SELECT TYPE进行一一判断。**

下面就是通过Fortran语法进行的面向对象编程，其中首先通过模块abs_vector声明了一个抽象类，并绑定了四种基本的计算方法：add,sub,dot,cross。

接着我们声明了一个二维子类，并重载了四种方法的计算方式。可以看到，只有第一个形参a才能声明为vec_2d，其余后面的参数必须利用SELECT TYPE。

```fortran
MODULE abs_vector
    IMPLICIT NONE
    TYPE,ABSTRACT,PUBLIC::vec
        CONTAINS
        !绑定方法 抽象方法
        PROCEDURE(add_value),PUBLIC,PASS,DEFERRED::add
        PROCEDURE(sub_value),PUBLIC,PASS,DEFERRED::sub
        PROCEDURE(dot_value),PUBLIC,PASS,DEFERRED::dot
        PROCEDURE(cross_value),PUBLIC,PASS,DEFERRED::cross
    END TYPE vec
    !============+==============!
    ABSTRACT INTERFACE
    SUBROUTINE add_value(a,b)
    IMPORT vec
    IMPLICIT NONE
    CLASS(vec),INTENT(INOUT)::a,b
    END SUBROUTINE add_value
    END INTERFACE
    !============-==============!
    ABSTRACT INTERFACE
    SUBROUTINE sub_value(a,b)
    IMPORT vec
    IMPLICIT NONE
    CLASS(vec),INTENT(INOUT)::a,b
    END SUBROUTINE sub_value
    END INTERFACE
    !============*==============!
    ABSTRACT INTERFACE
    REAL FUNCTION dot_value(a,b)
    IMPORT vec
    IMPLICIT NONE
    CLASS(vec),INTENT(INOUT)::a,b
    END FUNCTION dot_value
    END INTERFACE
    !============/==============!
    ABSTRACT INTERFACE
    SUBROUTINE cross_value(a,b)
    IMPORT vec
    IMPLICIT NONE
    CLASS(vec),INTENT(INOUT)::a,b
    END SUBROUTINE cross_value
    END INTERFACE
    !私有化方法
PRIVATE::add_value,sub_value,dot_value,cross_value

END MODULE abs_vector

!二维情况
MODULE vector_2d
    USE abs_vector
    IMPLICIT NONE
    TYPE,PUBLIC,EXTENDS(vec)::vec_2d
        REAL::x
        REAL::y
        CONTAINS
        !重载方法
        PROCEDURE,PUBLIC,PASS::add=>add_value2d
        PROCEDURE,PUBLIC,PASS::sub=>sub_value2d
        PROCEDURE,PUBLIC,PASS::dot=>dot_value2d
        PROCEDURE,PUBLIC,PASS::cross=>cross_value2d
        PROCEDURE,PUBLIC,NOPASS::set=>set_value2d

    END TYPE
    PRIVATE::set_value2d,add_value2d,sub_value2d,dot_value2d,cross_value2d
    CONTAINS
    TYPE(vec_2d) FUNCTION set_value2d(a,b)
        REAL::a,b
        set_value2d%x=a
        set_value2d%y=b
    END FUNCTION

    SUBROUTINE add_value2d(a,b)
        CLASS(vec_2d),INTENT(INOUT)::a
        class(vec),intent(inout)::b
        !注意除第一个参数意外的形参均要通过
        !SELECT TYPE进行数据类型的选择
        SELECTTYPE(b)
        TYPE IS (vec_2d)
        a%x=a%x+b%x
        a%y=a%y+b%y
        ENDSELECT
    END SUBROUTINE

    SUBROUTINE sub_value2d(a,b)
        CLASS(vec_2d),INTENT(INOUT)::a
        CLASS(vec),INTENT(INOUT)::b
        SELECTTYPE(b)
        TYPE IS (vec_2d)
        a%x=a%x-b%x
        a%y=a%y-b%y
        ENDSELECT
    END SUBROUTINE

    REAL FUNCTION dot_value2d(a,b)
        CLASS(vec_2d),INTENT(INOUT)::a
        CLASS(vec),INTENT(INOUT)::b
        SELECTTYPE(b)
        TYPE IS (vec_2d)
        dot_value2d=a%x*b%x+a%y*b%y
        ENDSELECT
    END FUNCTION

    SUBROUTINE cross_value2d(a,b)
        CLASS(vec_2d),INTENT(INOUT)::a
        CLASS(vec),INTENT(INOUT)::b
        SELECTTYPE(b)
        TYPE IS (vec_2d)
        a%x=a%x-b%y
        a%y=a%y-b%x
        ENDSELECT

    END SUBROUTINE

END MODULE vector_2d

!============test================!
PROGRAM test
    USE abs_vector
    USE vector_2d
    IMPLICIT NONE
    TYPE(vec_2d)::a,b,c
    a=a%set(1.,2.)
    b=b%set(3.,4.)
    WRITE(*,*)a
    WRITE(*,*)b
    CALL a%add(b)
    WRITE(*,*)a
    CALL a%sub(b)
    WRITE(*,*)a
    CALL a%cross(b)
    WRITE(*,*)a
    WRITE(*,*)a%dot(b)
END PROGRAM
```

总结：Fortran写纯正OPP最好的方式是——不写。