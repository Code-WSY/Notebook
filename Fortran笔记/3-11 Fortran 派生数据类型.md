# 11 派生数据类型

## 11.1 派生数据类型简介

Fortran 的内置数据类型：整型、实型、复数型、逻辑型以及字符型。

Fortran 还允许创建自己的数据类型，以扩展语言的功能，或者简化某些特定问题的求解。

用户自定义数据类型可以把任何数值和元素组合在一起， 但每个元素必须是Fortran内置数据类型或者已定义的用户自定义类型。

因为用**户自定义数据类型必须由内置数据类型派生而来**， 因此它们被称为**派生数据类型**。

格式：

```fortran
TYPE[::]type_name
component definitions
...
END TYPE [TYPE_name]
!两个冒号以及END TYPE后面的类型名是可选的。
!一个派生数据类型中可以定义的元素个数没有限制。
```

示例：编写一个评分程序。

1. **定义一个名为person的数据类型**

   ```fortran
   TYPE::person
   	CHARACTER(len=14)::first_name
   	CHARACTER::middle_initial
   	CHARACTER(len=14)::last_name
   	CHARACTER (len=14)::phone
   	INTEGER::age
   	CHARACTER::sex
   	CHARACTER(len=11)::ssn
   END TYPE person
   ```

2. **声明此类型变量**

   ```fortran
   TYPE (person)::john,jane
   TYPE (person),DIMENSION（100)::people
   ```

3. **也可创建某个派生数据类型的无名常量**

   ```fortran
   Nike=person('Nike','R','Jone','123-456','21','M','123-45-6789')
   ```

   为了创建此类常量， 需要使用**结构构造器**。结构构造器由**类型名**(person)构成，且**类型名后随括号括住的派生数据类型的元素**， 其中元素出现的**顺序就是定义该派生类型时元素出现的顺序**。

4. **派生数据类型用作其他派生数据类型的元素**

   示例：评分程序可以含有一个名为grade_info 的派生数据类型，而此类型又可以含有上面已经定义的person 类型元素。

   ```fortran
   TYPE::grade_info
   TYPE(person)::student
   INTEGER::num_quizzes
   REAL,DIMENSION(10)::quiz_grades
   INTEGER::num_exams
   REAL,DIMENSION (10)::exam_grades
   INTEGER::final_exam_grade
   REAL::average
   END TYPE
   TYPE(grade_info),DIMENSION(30)::class
   ```

## 11.2 派生数据类型的使用

派生数据类型中的每个元素都可以像使用同种类型的其他变量一样独立使用：如果该元素是整型，那么就可以像使用其他整型变量一样使用它；其他类型也相同。

元素是由**元素选择器**（也称为组件选择器）指定,格式：

1. 一般格式

   ```fortran
   wsy%age=23
   !变量wsy的age元素设置为23
   ```

2. 数组元素

   ```fortran
   class(5)%final_exam_grade=100
   !class数组中第五个元素的final_exam_grade设置为100
   ```

3. 派生数据类型中的派生数据类型的元素

   ```fortran
   class(5)%student%age=24
   !class数组中的第五个元素也为派生数据，其age元素被设置为24
   ```

## 11.3 派生数据类型的输入与输出

1. 如果一个派生数据类型的变扯包括在READ语句中， 那么数据必须**按照该类型中定义元素的顺序来输入**。如果READ语句使用了I/O格式， 那么**格式描述符必须与变量中元素的顺序和类型相匹配**。
2. 如果派生数据类型的某个变量包括在了WRITE语句中， 那么该变量默认每个元素都会被以定义的顺序输出。如果WRITE语句使用I/O格式，那么**格式描述符必须和变量中元素的类型及顺序相匹配**。

示例：使用非格式化I/O及格式化I/O输出一个person类型变量。

```fortran
PROGRAM test_io
IMPLICIT NONE
!定义派生数据
TYPE::person
	CHARACTER(len=14)::first_name
	CHARACTER::middle_initial
	CHARACTER(len=14)::last_name
	CHARACTER (len=14)::phone
	INTEGER::age
	CHARACTER::sex
	CHARACTER(len=11)::ssn
END TYPE person
!声明变量：
TYPE(person)::john
john=person('John','R','Jones','323-6439',21,'M','123-45-6789')
!用自由格式输出变量
WRITE(*,*)'Free format: ',john
!用I/O格式输出变量
WRITE(*,100)john
100 FORMAT('formatted I/O: ',/,4(1X,A,/),1X,I4,/,1X,A,/,1X,A)
END PROGRAM test_io
```

## 11.4 在模块中声明派生数据类型

对于使用派生数据类型的大程序， 可以在一个模块中声明所有数据类型的定义， 然后在每个需要访问该派生数据类型的过程中使用该模块。

当Fortran 编译器为派生数据类型的变量分配内存空间时，编译器并不需要为该类型变量的每个元素分配连续的空间。相反， 它们在内存中的位置是随机的，只要能够保证I/O操作时元素之间保持原有的顺序即可。这种自由性被有意识的加入到Fortran 标准中， **以便在大规模并行计算机上能最大限度地优化内存分配。**

如果由于某些原因， 一个派生数据类型的元素必须占据连续的内存空间， 那么可以在类型定义中使用SEQUENCE语句：

```fortran
TYPE :: vector
SEQUENCE
REAL :: a
REAL :: b
REAL :: c
END TYPE
```

## 11.5 从函数返回派生类型

当且仅当一个函数有**显式接口**时，可以创建有派生数据类型的函数。

创建此类函数最简单的方法是**将此函数放在一个模块里**， 然后使用USE 语句访问该模块。

**示例**：创建一个包含二维矢量数据类型的模块以及两个完成矢量加减法的函数。

1. **算法描述**
   - 提示用户输入矢量v1
   - 读取v1
   - 提示用户输入矢裁v2
   - 读取v2
   - 输出两个矢量和
   - 输出两个矢量差

2.  **Fortran代码**

   ```fortran
   MODULE vector
       IMPLICIT NONE
       SAVE
       TYPE::vec
           REAL::x
           REAL::y
           REAL::z
       END TYPE
       CONTAINS
       !v1+v2
       FUNCTION vec_add(v1,v2)
           TYPE(vec)::vec_add
           TYPE(vec),INTENT(IN)::v1,v2
           vec_add%x=v1%x+v2%x
           vec_add%y=v1%y+v2%y
           vec_add%z=v1%z+v2%z
       END FUNCTION vec_add
   	!v1-v2
       FUNCTION vec_sub(v1,v2)
           TYPE(vec)::vec_sub
           TYPE(vec),INTENT(IN)::v1,v2
           vec_sub%x=v1%x-v2%x
           vec_sub%y=v1%y-v2%y
           vec_sub%z=v1%z-v2%z
       END FUNCTION vec_sub
   END MODULE vector
   
   PROGRAM test_vec
       USE vector
       IMPLICIT NONE
       TYPE(vec)::v1,v2
       WRITE(*,*)'INPUT V1: '
       READ(*,*)v1
           WRITE(*,*)'INPUT V2: '
       READ(*,*)v2
       WRITE(*,1)vec_add(v1,v2)
       1 FORMAT('V1+V2= ',3F10.2)
       WRITE(*,2)vec_sub(v1,v2)
       2 FORMAT('V1-V2= ',3F10.2)
   END PROGRAM test_vec
   ```
   

## 11.6 生数据类型的动态内存分配

声明派生数据类型的**变量或数组**时可以使用ALLOCATABLE 属性，即可以得到动态分配和回收：

1.  

   ```fortran
   TYPE :: personal_info
   CHARACTER(len=12)::first
   CHARACTER::middle
   CHARACTER(len=12)::last
   CHARACTER(len=26)::street
   CHARACTER(len=12)::city
   CHARACTER(len=2)::state
   INTEGER::zip
   END TYPE personal_info
   ```

2. 声明和分配变量

   - ```fortran
     TYPE(personal_info),ALLOCATABLE::person
     ALLOCATE(PERSON,STAT=istat)
     ```

   - ```fortran
     TYPE(personal_info),DIMENSION(:),ALLOCATABLE::people
     ALLOCATE(people(1000),STAT=istat)
     ```

## 11.7 参数化派生数据类型

正如Fortran 允许多种整数或实数KIND, Fortran允许用户**使用参数定义派生数据类型**。
这种方式叫作**参数化派生数据类型**。

**注意，参数化派生数据类型中声明过程指针时，引用的过程中如果包含该参数化派生数据类型，必须声明(*)默认值进行处理。因为其只能在运行时选取你需要的类型，子例程或者函数中只能用默认值**

两种参数可以用来定义派生数据类型：

- **KIND**: 类别型参数(编译时已知)
- **LEN**:  长度类型参数(运行时获取)

表示**种类号**和**元素长度**的形式参数值（也称为**哑元值**）在**类型名称后面的括号中指定**，然后使用这些形式参数值来定义**派生类型中的实际类型**和**长度**。**如果没有指定形式参数值**， 那么派生数据类型就会由在**类型定义中所指明的默认值来决定**。

示例：声明一个带有**KIND**和**LEN**的矢量类型

```fortran
MODULE test
    IMPLICIT NONE
TYPE::vector(kinds,lens)
	INTEGER,KIND::kinds= KIND(0.0D0)           !默认为双精度
	INTEGER,len::lens=3                        !默认为三个元素
	REAL(kinds),DIMENSION(lens)::v      	   !参数化矢量
END TYPE vector
END MODULE test

PROGRAM main
    USE test
    IMPLICIT NONE
    TYPE(vector)::objects            !默认
    READ(*,*)objects%v
    WRITE(*,*)objects%v
END PROGRAM

```

内置函数KIND, 它可以返回一个给定的常量或变量的类别号。这个函数可以用于**判定编译器所使用的类别号**。

```fortran
TYPE(vector(KIND(0.),3))::v1          !指定类型和长度
TYPE(vector)::v2                      !默认类型和长度
```

类型声明产生一个含有20个双精度元素矢量的派生数据类型：

```fortran
TYPE(vector(KIND(0.0D0),20)):v3   
```

声明产生一个长度为100的矢量数组， 其中每个矢量都含有20个双精度元素：

```fortran
TYPE(vector(KIND(0.D0),20)),DIMENSION(100):: v4
```

可以将派生数据类型声明为可分配的，使得在分配内存时才明确每个元素的长度。其长度是在实际执行ALLOCATE 语句时才确定：

```fortran
TYPE(vector(KIND(0.),:)),ALLOCATABLE::v5
```

## 11.8 类型扩展(EXTENDS)

没有SEQUENCE或BIND(C)属性的自定义类型是**可扩展的**。

这表示，一个已存在的用户自定义类型可以作为更大或者更广泛的类型定义基础来使用。

示例：

```fortran
!定义一个二维的点坐标
TYPE::point
	REAL::x
	REAL::y
END TYPE

!三维的点的坐标数据就可以经过扩展一个现存二维点坐标数据类型(父类)得到：
TYPE,EXTENDS(point)::points3d
	REAL::z
END TYPE
!声明三维点数据
TYPE(points3d)::p
```

## 11.9 类型绑定过程

Fortran 还允许将**程序**与**派生数据类型**明确关联(**绑定**)。这些过程**只能**与其中定义的派生数据类型变量一起使用。通过添加CONTAINS 语句来定义类型，并在语句中声明绑定，来创建类型绑定Fortran过程。

示例：开发一个函数，实现对point类型两个数据项的加法。

```fortran
TYPE::point
	REAL::x
	REAL::y
	CONTAINS
		PROCEDURE,PASS::add   !声明add过程，绑定到这一数据类型
END TYPE 
```

- 如果p是point类型变量， 那么add过程的引用就可以写作p%add(...)
- PROCEDURE：对add进行类型声明，即是一个程序。
- PASS与NOPASS意味着**用来调用绑定过程的变量**是否会自动作为函数的第一个参数传递给绑定过程

过程add需要当作为类型定义语句在同一模块中定义：

```fortran
MODULE point_module
    IMPLICIT NONE
    TYPE::point
        REAL::x
        REAL::y
        CONTAINS
            PROCEDURE,PASS::add
            !PROCEDURE,NOPASS::add
    END TYPE
    CONTAINS
    !当某个对象的具体类型在运行时才能确定时
    !我们可以用CLASS关键字（这里的CLASS指的是一组有继承关系的类型）定义一个具有多态功能的指针或可分配对象
        TYPE(point) FUNCTION add(a,b)
            CLASS(point)::a,b
                add%x=a%x+b%x
                add%y=a%y+b%y
        END FUNCTION
END MODULE

PROGRAM main
    USE point_module
    IMPLICIT NONE
    TYPE(point)::a,b
    READ(*,*)a,b
    WRITE(*,*)a%add(b)
    !WRITE(*,*)a%add(a,b)
END PROGRAM

```

- PASS：指明调用此过程的point类型变量(如上述a%add中的a)会被当成**第一调用参数**自动传递到这一过程，不论何时调用。
- NOPASS：绑定过程不会自动获得用来当成调用参数的变量。必须在调用中显式地给出第一参数来调用绑定函数。如：a%add(a,b)

## 11.10 ASSOCIATE结构

ASSOCIATE 结构允许在一个代码段的执行过程中，**临时**将变量或表达式和某个名字关联。

这种结构对于**简化拥有长名字和多个下标的变量**或**表达式的引用**非常有用。

格式:

```fortran
[name:]ASSOCIATE(association_list)
Statement 1
Statement 2
...
Statement n
END ASSOCIATE [name]
!Association_ list是一个或多个下列形式关联的集合。
!如果有多个关联出现在列表中， 它们之间用逗号（,）分隔。
```

```fortran
assoc_name=>variable,array element or expression
```

示例：雷达追踪

```fortran
!设置雷达目标位置的派生数据类型
TYPE::trackfile
	REAL::x
	REAL::y
	REAL::dist
	REAL::bearing
END TYPE trackfile
TYPE(trackfile),DIMENSION(1000)::active_tracks
!设置雷达位置的派生数据类型
TYPE::radar_loc
	REAL::x
	REAL::y
END TYPE radar_loc
TYPE(radar_loc)::my_radar
!计算目标的距离：
DO i=1,n_track
	active_tracks(i)%dist = SQRT((my_radar%x-active_tracks(i)%x)**2+(my_radar%y-active_tracks(i)%y)**2)
END DO
!如果用ASSOCIATE代替：
DO i=1,n_track
	ASSOCIATE(x=>active_tracks(i)%x,&
			  y=>active_tracks(i)%y,&
		   dist=>active_tracks(i)%dist)
	dist = SQRT((my_radar%x-x**2+(my_radar%y-y)**2)
	END ASSOCIATE   
END DO
```

ASSOCIATE结构并不是必不可少，但是它对于简化和突出算法有帮助。

## 11.11 小结

### 11.11.1 遵循原则

对于使用派生数据类型的大型程序来说，应该在模块中声明每个数据类型，然后在需要访问派生数据类型的过程中使用该模块。

### 11.11.2 语法小结

1. **ASSOCIATE结构**

   ```fortran
   !格式：
   [name:] ASSOCIATE(association_list)
   Statement 1
   ...
   Statement n
   END ASSOCIATE [name]
   !示例：
   ASSOCIATE(x => target(i)%state_vector%x,&
             y => target(i)%state_vector%y )
        dist(i)= SQRT(x**2 + y**2)
   END ASSOCIATE
   ```

   ASSOCIATE 结构允许程序员用结构体中更短的名字来实现用长名字访问一个或多个变量的操作。由于单个变量名都不是太烦琐，所以带有ASSOCIATE的等式可以非常紧凑。

2. **派生数据类型**

   ```fortran
   !格式：
   TYPE[::] type_name
   component_1
   ....
   component_n
   CONTAINS
   	PROCEDURE[,[NO]PASS]:: proc_namel[,proc_name2,...]
   END TYPE [type_name]
   TYPE(type_name)::varl(,var2,...)
   !示例：
   MODULE test
       IMPLICIT NONE
   INTEGER,PARAMETER::single=4,double=8
   TYPE::state_vector
   	LOGICAL::valid
   	REAL(kind=single)::x
   	REAL(kind=single)::y
   	REAL(kind=double)::time
   	CHARACTER(len=12)::id
   END TYPE state_vector
   END MODULE test
   
   PROGRAM main
       USE test
       IMPLICIT NONE
       TYPE(state_vector)::objects
       READ(*,*)objects
       WRITE(*,*)objects
   END PROGRAM
   
   ```

   派生数据类型是包含**内置数据类型和已定义派生数据类型联合体的结构**。通过TYPE…END TYPE结构定义这种类型，用TYPE语句声明这种类型的变量。

   派生数据类型中**绑定过程**仅在Fortran 2003和后续版本中有效。

3. **NO PASS属性**

   ```fortran
   !格式：
   TYPE :: name
   variable definitions
   CONTAINS
   PROCEDURE,NOPASS : : proc_name
   END TYPE
   
   !示例：
   TYPE :: point
   REAL :: x
   REAL :: y
   CONTAINS
   PROCEDURE,NOPASS :: add
   END TYPE
   ```

   NOPASS属性意味着调用绑定过程的变量不会自动作为第一调用参数传递给过程。

4. **PASS属性**

   ```fortran
   !格式：
   TYPE :: name
   variable definitions
   CONTAINS
   PROCEDURE,PASS:: proc name
   END TYPE
   !示例：
   TYPE :: point
   REAL :: x
   REAL :: y
   CONTAINS
   PROCEDURE,PASS::add
   END TYPE
   ```

   PASS 属性意味着用来调用绑定过程的变量会自动作为函数的第一个参数传递给绑定过程，这是默认的绑定过程参数传递方式。
