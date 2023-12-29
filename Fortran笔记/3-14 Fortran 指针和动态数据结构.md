# 14 指针和动态数据结构

## 14.1 指针和目标变量

Fortran包含另一种类型的变量，该变量不包含数据。相反，它包含的是另一变量在内存中的地址，即这另一变量在内存中实际存储的位置。因为这种类型的变量指向另外的变量，所以又称为指针。

指针和普通变量都有名字，但是指针保存的是普通变量的地址，而普通变量保存的是数据值。

指针主要用于变量和数组必须在程序执行的过程中动态创建和销毁的情况，此时在程序执行前并不知道这些变量和数组在哪，在程序执行过程中才知道需要多少个给定类型的变量。

**POINTER**

一般格式：

```fortran
REAL,POINTER::p1
REAL::p2
POINTER::p2
```

注意，即使指针不包含指针类型的任何数据，也必须声明指针的类型。实际上，它包含的是所声明的某种类型的变量的地址。指针只允许指向与其声明类型一致的变量。任何将指针指向不同类型变量的做法都会产生编译错误。

指向派生数据同样需要声明：

```fortran
TYPE(vector),POINTER:: vector_pointer
```

指针同样也可以指向数组。使用**延迟形状数组**规范声明指向数组的指针，即**指定数组的维数，但是数组每维的实际宽度用冒号指明**：

```fortran
INTEGER,DIMENSION (:),POINTER::ptrl
REAL,DIMENSION (:,:),POINTER::ptr2
```

第一个指针可以指向任何一维的整型数组， 第二个指针可以指向任何二维的实型数组。

**TAGET**

TARGET是一种**数据对象**，**其地址在使用指针时可用**。可以通过在Fortran 变量或数组的类型定义语句中包括TARGET 属性来将其声明为目标变量（推荐做法）， 也可以将变量或数组列在单独的TARGET 语句中。

```fortran
REAL,TARGET::al=7
INTEGER,DIMENSION(10),TARGET::int_array
!或
REAL::a1=7
INTEGER,DIMENSION(10)::int—array
TARGET::a1,int_array
```

它们声明了一个实型的标量a1 和一个一维的整型数组int_array。可以使用任何实型标量指针（如上面声明的p1) 指向变量a1，使用任何整型的一维指针（如上面声明的ptr1) 指向int_array。

### 14.1.1 指针赋值语句

可以将指针关联到指定的目标变量。指针赋值语句的形式如下：

```fortran
pointer=>target
!切记 指针的任何赋值语句用=>，任何形式下不要用=，会出现内存引用错误
```

其中pointer是指针的名字，而target是和指针同一类型变量或数组的名字。

当执行此语句时，目标变量的内存地址就保存在了指针中。指针赋值语句之后，任何对该指针的引用实际上都是对保存在目标变量中的数据的引用。

示例：

```fortran
PROGRAM test_ptr
IMPLICIT NONE
REAL,POINTER :: p
REAL,TARGET :: t1 =10., t2 = -17.
p => t1
WRITE (*, *) 'p, t1, t2 = ',p,t1,t2
p => t2
WRITE(*,*) 'p, t1, t2 = ', p,t1,t2
END PROGRAM test_ptr
!ouput
 p, t1, t2 =    10.0000000       10.0000000      -17.0000000
 p, t1, t2 =   -17.0000000       10.0000000      -17.0000000
```

同样也可以通过指针赋值语句将一个指针赋值个另一个指针：

```fortran
pointer1=>pointer2
```

示例：

```fortran
PROGRAM test_ptr2
IMPLICIT NONE
REAL, POINTER :: p1, p2
REAL, TARGET  :: t1 = 10., t2 = -17.
p1 => t1
p2 => p1
WRITE (*,'(A,4F8.2)') ' p1, p2, t1, t2 = ',p1, p2, t1, t2
!10 10 10 -17
p1 => t2
WRITE (*,'(A,4F8.2)') ' p1, p2, t1, t2 = ',p1, p2, t1, t2
!-17 10 10 -17
END PROGRAM test_ptr2
!output
 p1, p2, t1, t2 =    10.00   10.00   10.00  -17.00
 p1, p2, t1, t2 =   -17.00   10.00   10.00  -17.00
```

### 14.1.2 指针关联状态

指针关联状态指的是指针当前是否指向一个有效的目标变量。有三种可能的状态： **未定义**，**相关联**以及**未关联**。

- 在**数据类型声明语句中声明指针时**，其指针关联状态是**未定义**。

- 指针和目标变量通过指针赋值语句关联起来，那么其关联状态是**相关联**。

- 如果指针后来和其目标变量断开，并且没有和新的目标变量关联，那么其关联状态是空状态(**未关联**)。

1. **指针与目标变量断开(NULLIFY)：**

   执行指针赋值语句就可以与一个目标变量断开的同时和另一个目标变量关联。此外，可以通过执行NULLIFY语句将指针和所有目标变量断开：

	```fortran
	NULLIFY(ptr1[,ptr2,...])
	```

	仅当指针和某目标变量关联时，该指针才可引用相应目标变量。当指针没有和目标变量关联时，任何对指针的使用都会导致错误，使得程序退出。

2. **获取关联信息(ASSOCIATED)：**

	```fortran
	status=ASSOCIATED(pointer)
	```
	如果pointer和某个目标变量关联，那么该函数返回真值，如果pointer没有和任何目标变量关联，那么函数返回假值。

	```fortran
	status=ASSOCIATED(pointer,target)
	```

	如果pointer和某个包含在函数中的特定目标变量关联，那么此函数返回真值，否则返回假。

**在程序单元中创建指针时就让其未关联或给其赋值。这样可以消除所有可能的未定义状态相关的不确定性。**

Fortran 同样也提供了一个内置函数NULL()，此函数可以在声明指针的同时（或者在程序执行的任何过程中）将其置为空状态。因此，指针可以按照如下方式声明和取空状态：

```fortran
REAL,POINTER :: p1 => NULL(),p2 =>NULL()
INTEGER,POINTER :: i1 => NULL()
```

示例：NULL函数以及ASSOCIATED 内置函数的使用

```fortran
PROGRAM test_ptr3
IMPLICIT NONE
!定义指针并设置为空状态
REAL,POINTER :: p1=>null(), p2=>null(), p3=>null()
REAL,TARGET  :: a = 11., b = 12.5, c = 3.141592
!检查指针是否是关联的
WRITE (*,*) ASSOCIATED(p1)
p1 => a ! p1指向a
p2 => b ! p2指向b
p3 => C ! p3指向c
!检查指针是否是关联的
WRITE (*,*) ASSOCIATED(p1)
!指针pl是否指向变量b
WRITE (*,*) ASSOCIATED(p1,b)
END PROGRAM test_ptr3
```

## 14.2 在赋值语句中使用指针

只要指针出现在需要数值的Fortran表达式中时，就是使用指向的目标变量的值来代替指针本身。这一过程就是指针的断开引用。

示例：

```fortran
PROGRAM test_ptr4
IMPLICIT NONE
REAL, POINTER :: p1 =>null(),p2=>null(), p3=>null( )
REAL, TARGET  :: a = 1, b = 2, c
p1 => a ! pl指向a
p2 => b ! p2指向b
p3 => c ! p3指向c
p3 = p1+p2 !犹如c = a + b=3
WRITE(*,*) 'p3 = ', p3
p2 => p1 ! p2指向a
p3 = p1 + p2 !犹如c = a + a=2
WRITE(*,*) 'p3 = ', p3
p3 = 2*p1 !犹如c = 2a=2
!P3与c断开
p3 => p1 ! p3指向a=1
WRITE(*,*) 'p3 = ', p3
!1 2 2
WRITE(*,*) 'a, b,c = ', a, b, c
END PROGRAM test_ptr4
```

现在来展示使用指针改善程序性能的方法。假设程序要交换两100X100个元素的实型数组array1和array2。为了交换这两个数组，通常使用的是下面的代码：

```fortran
REAL, DIMENSION(100,100) :: arrayl,array2,temp
...
temp=arrayl
...
arrayl=array2
array2=temp
```

代码足够简单，但是注意，每条赋值语句我们都要移动10000个实型变量。所有这些**移动需要大量的时间**。

相反， 可以使用指针完成相同的操作，仅需要交换目标数组地址即可。

```fortran
REAL, DIMENSION(100,100), TARGET :: array1, array2
REAL, DIMENSION(:,:),POINTER :: p1, p2, temp
p1 => array1
p2 => array2
...
temp=> p1
p1  => p2
p2  => temp
```

指针方法仅需要交换地址，而不是完整的10000个数组元素。这要比前一个例子高效的多。

在排序或交换大型数组或派生数据类型时，交换指向数据的指针要比操作数据本身高效得多。

## 14.3 使用数组指针

指针可以指向数组，也可以指向标量。指向数组的指针必须声明其将指向的数组的类型及维数，但是**不需要声明每一维的宽度。**

格式：

```fortran
REAL, DIMENSION(1000,1000), TARGET :: array
REAL, DIMENSION(:,:), POINTER :: pointer
pointer => array
```

指针不仅能指向数组而且能指向数组子集（部分数组）。

示例：

```fortran
PROGRAM array_ptr
IMPLICIT NONE
INTEGER :: i
INTEGER,DIMENSION(16), TARGET :: info = [(i,i=1,16)]
INTEGER,DIMENSION(:), POINTER :: ptr1, ptr2, ptr3, ptr4, ptr5
ptr1 => info
ptr2 => ptr1(2::2)
ptr3 => ptr2(2::2)
ptr4 => ptr3(2::2)
ptr5 => ptr4(2::2)
!1 2 3...16
WRITE (*,'(A,16I3)') ' ptrl = ', ptr1
!2 4 6...16
WRITE (*,'(A,16I3)') ' ptr2 = ', ptr2
!4 8 12 16
WRITE (*,'(A,16I3)') ' ptr3 = ', ptr3
!8 16
WRITE (*,'(A,16I3)') ' ptr4 = ', ptr4
!16
WRITE (*,'(A,16I3)') ' ptrS = ', ptr5
END PROGRAM array_ptr
```

## 14.4 使用指针的动态内存分配（ALLOCATE）

指针最强大的功能之一是**能够使用它们在任何需要的时候动态创建变量或数组**，**还能在使用完毕后，释放动态变量或数组所使用的空间。**这种过程类似于创建可分配数组的过程。

使用ALLOCATE语句**分配内存**，使用DEALLOCATE语句释放内存。ALLOCATE语句的形式和用于可分配数组的ALLOCATE语句形式相同。具体形式如下：

```fortran
ALLOCATE(pointer(size), [...,] STAT=status)
```

**其中pointer是指向所创建的变量或数组的指针名**，如果创建的对象是数组，那么size是维数，status是操作的结果。

如果分配成功，那么status为0。如果失败，那么status变量的值会是一个与处理器相关的正数。STAT是可选的，但是通常都要使用它，因为如果没有STAT，那么分配内存失败会让程序意外终止。

此语句创建了一个**未命名的数据对象**，它有指定的**长度**和**指针类型**，并且让指针指向该对象。

**因为新的数据对象是未命名的，所以只能通过指针来访问。**语句执行后，指针的关联状态就变为相关联。如果在ALLOCATE语句执行前，指针和另一个数据对象关联，那么该关联丢失。

如果所有指向该片内存的指针被置为空或和其他目标变量再关联的，那么程序就不能再访问该数据对象了。

对象仍在内存中存在，但是无法再用了。因此如果编写带有指针的程序时不小心，就会造成内存中充满了此类无用碎片。这种无用内存通常被称为“内存泄漏＂。

问题的一个症状就是随着程序的执行，内存碎片变得越来越大，直到填满了整个计算机内存或者用完了所有可用的内存。

```fortran
PROGRAM mem_leak
IMPLICIT NONE
INTEGER:: i,istat
!这里如果不指向空态而直接声明变量在ASSOCIATED判断时会出错 
INTEGER,DIMENSION (:),POINTER:: ptr1=>NULL(),ptr2=>NULL()
!检查ptrs关联状态
WRITE (*,'(A,2L5)') ' Are ptrl,ptr2 associated?', &
ASSOCIATED(ptr1), ASSOCIATED(ptr2)
!分配和初始化内存
ALLOCATE (ptr1(1:10), STAT=istat)
ALLOCATE (ptr2(1:10), STAT=istat)
ptr1 =[(i,i = 1,10)]
ptr2 =[(i,i = 11,20)]
!检查ptr关联状态．
WRITE (*,'(A,2L5)') ' Are ptrl, ptr2 associated?', &
ASSOCIATED(ptr1), ASSOCIATED(ptr2)
WRITE (*,'(A,10I3)') ' ptrl = ', ptr1
WRITE (*,'(A,10I3)') ' ptr2 = ', ptr2
!将ptr2指向prt1 prt2原本数据内存丢失
ptr2 => ptr1
WRITE (*,'(A,10I3)') ' ptrl = ', ptr1
WRITE (*,'(A,10I3)') ' ptr2 = ', ptr2
NULLIFY(ptr1)
!当程序使用完ALLOCATE语句分配的内存后，应该使用DEALLOCATE语句收回内存。
!当用指针DEALLOCATE语句回收内存时，指向该片内存的指针同时变为无效。
DEALLOCATE(ptr2, STAT=istat)
END PROGRAM mem_leak
```

注意：**指针DEALLOCATE语句只能回收使用ALLOCATE语句所创建的内存**

如果语句中的指针碰巧指向不是使用ALLOCATE语句所创建的目标变量， 那么DEALLOCATE语句就会失败，程序会退出，除非指定了STAT。此类指针和其目标变量之间的关联可以通过使用NULLIFY语句来断开。

回收内存还有可能引发一个潜在的**严重问题**。假设两个指针ptr1和ptr2**同时指向同一己分配数组**。

如果在DEALLOCATE语句中**使用指针ptr1回收数组**，那么**该指针就置空**了。但是**ptr2不会置空**。它仍然指向数组所在的那片**内存**，即使该内存已经被其他程序为了其他目的而使用。如果ptr2指针用与读或写对应内存的数据，它或者读到不可预测值或者覆盖了用于其他目的的一些内存数据。

两种情况下，使用该指针都是灾难。如果回收了一块已分配的内存，那么所有指向该内存的指针都应无效或重赋值。**它们中的一个会因为DEALLOCATE语句的使用自动置空，但是其他的必须通过NULLIFY语句来置空。**

因此：**当释放某片内存空间时，记得总是置空或重赋值指向内存的所有指针。**它们中的一个会因为DEALLOCATE语句的使用自动置空，但是其他的必须通过NULLIFY语句置空，或通过指针赋值语句重赋值。

## 14.5 指针当作派生数据类型的元素

指针也可以作为派生数据类型的元素出现。**派生数据类型中的指针甚至可以指向所定义的派生数据类型**。

这一特点非常有用， 因为它允许我们在程序执行过程中**构建各种类型的动态数据结构**， 并使用连续指针将这些数据结构链接到一起。最简单的这种结构是**链表**， 它通过指针按照线性的方式连接为数值列表。

例如， 下面的派生数据类型包括**实数**和**指向另一个同种类型变量的指针**：

```fortran
TYPE :: real_value
REAL :: value
TYPE(real_value),POINTER :: p
END TYPE
```

### 14.5.1 链表

链表是**一系列派生数据类型的变量，指针从一个变量指向链表中的下一个变量**。最后一个变量的指针是空的，因为链表中该变量之后没有任何变量。

定义两个指针（明确地说，是head 和tail）用于指向链表中的第一个和最后一个变量。

链表要比数组灵活得多。

当编译程序时， 静态数组必须声明固定的大小。结果是，我们必须给每个数组预留很大的空间，以便处理需要处理的最大问题。这种最大的内存需求可以导致程序太大， 而在某些计算机上无法运行，同样也会导致在程序执行的大部分时间内的资源浪费。

使用可分配数组也无法彻底解决问题。可分配数组解决了内存浪费问题，因为它允许仅分配适用于特定问题所需大小的内存，但是我们必须在分配内存前清楚究竟这个特定问题在运行时需要多大的内存。

相反，链表允许我们随时添加元素，事先并不需要知道列表中最终会有多少元素。

**示例1：创建链表**

```fortran
!创建链表
PROGRAM linked_list
    IMPLICIT NONE
    !存储实数型值的派生数据类型
    TYPE ::real_value
        REAL::value
        TYPE(real_value),POINTER::p=>NULL()
    END TYPE
    !声明变量类型
    TYPE(real_value),POINTER::head=>NULL()  !指向链表的头结点
    CHARACTER(LEN=20)::filename             !输入数据文件名称
    INTEGER::nvals= 0                       !数据读取个数
    TYPE(real_value),POINTER::ptr=>NULL()   !临时指针
    TYPE(real_value),POINTER::tail=>NULL()  !指向链表的尾节点
    INTEGER::istat                          !状态：0表示成功
    CHARACTER(LEN=80)::msg                 !I/O消息
    REAL::temp                              !临时变量

    !获得数据文件的文件名
    WRITE(*,*)'Enter the file name with the data to be read: '
    READ(*,'(A20)')filename
    !打开数据文件
    OPEN(UNIT=8,FILE=filename,STATUS='OLD',ACTION='READ',IOSTAT=istat,IOMSG=msg)
    !判断是否打开成功
fileopen:IF(istat==0)THEN
    !读取文件
    input: DO
            READ(8,*,IOSTAT=istat,IOMSG=msg)temp               !读取数据放入temp临时变量中(实型变量)
           !如果中间有字符串跳过(5010)
            IF(istat /=0 .AND. istat/=-1) THEN
                CYCLE
            !到文件末尾后直接跳出循环程序
            ELSEIF(istat==-1)THEN
                WRITE(*,*)'READ OVER!'
                EXIT
            END IF
            !计数
            nvals=nvals+1
            
            IF(.NOT. ASSOCIATED(head))THEN           !刚刚开始，首末指针指向初始数据内存
                ALLOCATE(head,STAT=istat)
                tail => head
                head%value=temp
                head%p=>NULL()
            ELSE
                ALLOCATE(tail%p,STAT=istat)          !通过每个内存中的指针指向下一个内存并通过tail进行重新指向，head作为链式开头
                tail=>tail%p
                tail%value=temp
                tail%p=>NULL()
            END IF
            END DO input
    !输出文件
    ptr=>head
    WRITE(*,*)'value numeber is: ',nvals
    output: DO
                IF(.NOT. ASSOCIATED(ptr)) EXIT      !若指针无效 head，跳过
                WRITE(*,110)ptr%value
                110 FORMAT('value is : ',F10.2 )
                ptr=>ptr%p
            END DO output
        ELSE fileopen
            WRITE(*,'(A,I6)')'File open failed status = ',istat
            WRITE(*,*)msg

        END IF fileopen

END PROGRAM linked_list
```

**示例2：插入排序**

插入排序的原理为，在读入数据时就将其放置在列表中的正确位置。如果该值比列表中的所有原值都小，那么将它排在第一位。如果该值比列表中所有原值都大，那么把它排在最后。如果该值在最大最小之间，那么将它插入在列表中的合适位置。

```fortran
PROGRAM insertion_sort
    IMPLICIT NONE
    TYPE int_value
        INTEGER:: value
        TYPE(int_value),POINTER::next_value
    END TYPE
    TYPE(int_value),POINTER::head=>NULL(),tail=>NULL(),ptr1=>NULL(),ptr2=>NULL()
    TYPE(int_value),POINTER::ptr=>NULL()
    INTEGER::navls=0
    INTEGER::temp
    INTEGER::istat
    INTEGER::i
    CHARACTER(len=80)::msg
    CHARACTER(len=20)::filename
    WRITE(*,*)'Enter the file name with the data to be sorted: '
    READ(*,'(A20)')filename
    OPEN(UNIT=10,FILE=filename,STATUS='OLD',ACTION='READ',IOSTAT=istat)
    !OPEN
    ifopen: IF(istat==0)THEN
        input: DO
                READ(10,*,IOSTAT=istat,IOMSG=msg)temp
                IF(istat /= 0 .AND. istat/=-1 )THEN
                    CYCLE
                ELSEIF(istat == -1 )THEN
                    WRITE(*,*)'READ OVER'
                    EXIT
                END IF
                !WRITE(*,*)navls
                navls=navls+1
                ALLOCATE(ptr,STAT=istat)
                !WRITE(*,*)temp
                ptr%value=temp
                ptr%next_value=>NULL()
                !寻找放置列表的位置
                NEW:IF(.NOT. ASSOCIATED(head))THEN
                    head=>ptr
                    tail=>ptr
                    ELSE
                        front:IF(ptr%value<=head%value)THEN
                                ptr%next_value=>head
                                head=>ptr
                              ELSEIF(ptr%value>tail%value)THEN
                                tail%next_value=>ptr
                                tail=>ptr
                              ELSE
                                !ALLOCATE(ptr1,STAT=istat)
                                !ALLOCATE(ptr2,STAT=istat)
                                !ptr1=head
                                !ptr2=head%next_value
                                ptr1=>head
                                ptr2=>head%next_value
                            insert:DO
                                    IF(ptr1%value <= ptr%value .AND. ptr%value <= ptr2%value)THEN
                                            ptr%next_value=>ptr2
                                            ptr1%next_value=>ptr
                                            EXIT insert
                                    END  IF
                                    ptr1=>ptr2
                                    ptr2=>ptr1%next_value
                                   END DO insert
                              END IF front
                END IF NEW
               END DO input
                    ptr=>head
                    WRITE(*,'(A,I4)')'data number: ',navls
               output:DO
                         IF(.NOT. ASSOCIATED(ptr)) EXIT
                            WRITE(*,*)ptr%value
                            ptr=>ptr%next_value
                      END DO output
            ELSE
                    WRITE(*,'(A,I4,A)')'FAILD OPNE',istat,msg
            END IF ifopen
END PROGRAM
```

**切记！对于声明的指针一定要用ALLOCATE去创建内存**

## 14.6 指针数组

在Fortran中不可能声明指针数组。在指针声明中，DIMENSION属性指的是**指针指向的目标变量的维数，而不是指针本身的维数**。必须使用延迟形状规范来声明维数，而实际的大小是指针所关联的目标变量的大小。

在下面的例子中，**指针下标引用的是目标变量数组中的相应位置**，所以ptr(4)的值是6。

```fortran
REAL, DIMENSION (:), POINTER :: ptr
REAL, DIMENSION(5), TARGET :: tgt = [ -2, 5., 0., 6., 1 ]
ptr => tgt
WRITE (*,*) ptr(4)
```

在很多应用程序中，指针数组都很有用。幸运的是，可以通过派生数据类型来为这些应用程序创建指针数组。

在Fortran中，指针数组是非法的，但是创建**派生数据类型数组**却是再合法不过。

因此，可以**声明一个只包含指针的派生数据类型，然后创建该类型的数组**。

示例：声明一个**包含实型指针的派生数据类型数组**， 其中**每个指针都指向一个实型数组**。

```fortran
PROGRAM ptr_array
IMPLICIT NONE
!创建派生数据类型的指针
TYPE:: ptr
REAL,DIMENSION(:),POINTER::p
END TYPE
!派生数据类型的三元素指针数组
TYPE(ptr),DIMENSION(3):: p1
REAL,DIMENSION(4),TARGET :: a = [ 1., 2., 3., 4. ]
REAL,DIMENSION(4),TARGET :: b = [ 5., 6., 7., 8. ]
REAL,DIMENSION(4),TARGET :: c = [ 9., 10., 11., 12. ]
!给元素赋值，注意指针赋值用=>
p1(1)%p => a
p1(2)%p => b
p1(3)%p => C
WRITE(*,*) p1(3)%p  !=9 10 11 12
WRITE(*,*) p1(2)%p(3) !=7
END PROGRAM ptr_array
!OUTPUT:
9.000000 10.00000 11.00000 12.0000
7.000000
```

## 14.7 在过程中使用指针

指针可以作为过程的形参，也可以作为实参传递到过程中。

此外，函数的返回值也可以是指针。如果在过程中使用指针，那么就应该遵循下列原则：

1. 如果过程有POINTER或TARGET属性的形参，那么过程必须有**显式接口**。
2. 如果形参是指针，那么传给过程的实参必须是同一类型，类别和维度的指针。
3. 指针形参不能出现在**ELEMNETAL**过程中。

将指针传递给过程时一定要小心。随着程序越来越大，越来越灵活，经常面对在一个过程中分配指针、而在其他过程中使用、最后又在另外的过程中回收和置空指针的情况。在这种复杂的程序中，非常容易产生诸如企图使用未关联的指针，或者给已经在用的指针分配新数组此类的错误。

因此，**对于所有的ALLOCATE和DEALLOCATE语句来说，检查状态结果，以及使用ASSOCIATED函数检查指针的状态都是非常重要的**。

当使用指针将数据传给过程时，从指针本身的类型就能自动知道与指针相关联的数据的类型。如果指针指向一个数组，那么就能知道数组的维数，但不知道它的宽度或大小。

如果需要知道数组的宽度或大小，那么应该使用内置函数**LBOUND**和**UBOUND**来判定数组每一维的边界值。

**示例：从矩阵提取对角元素**

```fortran
!子例程：
SUBROUTINE get_diagonal(ptr_a,ptr_b,error)
!目的：从指针ptr_a指向的二维方形数组中提取出对角线元素，
!存储至ptr_b指向的一维数组中。
!出错定义
!0 -- 无错
!1 -- ptr_a与输入数据无关联
!2 -- ptr_b已经关联到输入数据
!3 -- ptr_a指向的数组不是正方形
!4 -- 不能为ptr_b 正确分配需要的内存空间
IMPLICIT NONE
!输入形参定义
INTEGER,DIMENSION(:,:),POINTER::ptr_a
INTEGER,DIMENSION(:),POINTER::ptr_b
INTEGER,INTENT(OUT)::error
!内部变量
INTEGER::i
INTEGER::istat
INTEGER,DIMENSION(2)::l_bound  !ptr_a的低端下标
INTEGER,DIMENSION(2)::u_bound  !ptr_b的高端下标
INTEGER,DIMENSION(2)::extent   !ptr_a数组下标越界
!检查出错
!error_1:
        IF( .NOT. ASSOCIATED(ptr_a))THEN
            error=1
!error_2:
        ELSEIF(ASSOCIATED(ptr_b))
            error=2
        ELSE
            l_bound=LBOUND(ptr_a)
            u_bound=UBOUND(ptr_b)
            extent=u_bound-l_bound+1
!error_3:
            IF(extent(1)/=extent(2))THEN
                error=3
            ELSE
                !如果每个事件都ok分配ptr空间
                ALLOCATE(ptr_b(extent(1)),STAT=istat)
!error_4:
                IF(istat/=0)THEN
                    error=4
                ELSE
!一切正常，提取对角元素
!correct:
                    DO i=1,extent(1)
                        ptr_b(i)=ptr_a(l_bound(1)+i-1,l_bound(2)+i-1)
                    END DO
!重置出错标志:
                    error=0
                END IF
            END IF
        END IF
END SUBROUTINE get_diagonal
```

### 14.7.1 使用指针的INTENT属性

如果INTENT属性出现在指针形参中，那么它**引用的是指针本身，而非它所指向的目标变量**。因此，如果子例程中有下列定义：

```fortran
SUBROUTINE test(xval)
REAL,POINTER,DIMENSION(:),INTENT(IN)::xval
```

那么在该子例程中**不能分配**指针xval, 也**不能回收**或者给它重**赋值**。不过，**可以修改指针目标变量的内容**。因此：

```fortran
xval(90:100)= -2.
```

在该子例程中是合法的，如果指针的目标变量至少有100个元素

### 14.7.2值为指针的函数

函数也可以返回指针的值。如果函数要返回指针，那么必须在函数定义中使用RESULT子句，而且RESULT变量必须声明为指针。

**示例：函数接收一个指向一维数组的指针，返回一个指向数组中第五个值的指针。**

```fortran
PROGRAM  test
    IMPLICIT NONE
    INTEGER,DIMENSION(:),POINTER::ptr
    INTEGER,ALLOCATABLE,DIMENSION(:),TARGET::arr
    INTEGER::i,num
    INTERFACE
        FUNCTION five_ele(ptr) RESULT(res_ptr)
                INTEGER,DIMENSION(:),POINTER,INTENT(IN)::ptr
                INTEGER,POINTER::res_ptr
        END FUNCTION
    END INTERFACE
    READ(*,*)num
    ALLOCATE(arr(num))
    arr=[(i,i=1,num,2)]
    ptr=>arr
    WRITE(*,*)five_ele(ptr)
END PROGRAM
FUNCTION five_ele(ptr) RESULT(res_ptr)
    INTEGER,DIMENSION(:),POINTER,INTENT(IN)::ptr
    INTEGER,POINTER::res_ptr
    INTEGER,TARGET::error= -1
    INTEGER::low,up
    low=LBOUND(ptr,1)
    up=UBOUND(ptr,1)

    IF((up-low<5))THEN
        res_ptr=>error
    ELSE
        res_ptr=>ptr(LBOUND(ptr,1)+4)
    END IF
END FUNCTION
```

## 14.8 过程指针

对千Fortran指针来说，也可以指向过程。

格式：

```fortran
PROCEDURE(proc), POINTER::p=>NULL()
```

此语句声明了一个指向过程的指针，该过程有着和proc过程一样的调用顺序，也**必须有一个显式接口**。
一旦声明了过程指针，那么就能按照和变量或数组一样的方式将过程赋给指针。

例如，假定子例程sub1有一个显式接口。那么指向sub1的指针可以声明为：

```fortran
PROCEDURE(sub1), POINTER::p=>NULL()
```

将过程赋给指针：

```fortran
P=>sub1
```

在这样赋值之后，下列两个子例程调用语句是等价的，结果相同：

```fortran
CALL sub1(a,b,c)
CALL p(a,b,c)
```

注意，这一指针对任何与接口sub1一样的子例程都可用。

例如，假设子例程sub1和sub2有同样的接口（**个数，顺序，类型以及调用参数的使用目的都一样**）。那么下列第一个对p的调用就是调用sub1, 而第二个就是调用sub2。

```fortran
P=>sub1
CALL p(a,b,c)
P=>sub2
CALL p(a,b,c)
```

示例：选择函数

```fortran
MODULE test_function
    IMPLICIT NONE
    CONTAINS
        REAL FUNCTION func1(x)
        IMPLICIT NONE
        REAL,INTENT(IN)::x
        func1=x
        END FUNCTION
        REAL FUNCTION func2(x)
        IMPLICIT NONE
        REAL,INTENT(IN)::x
        func2=EXP(x)
        END FUNCTION
        REAL FUNCTION func3(x)
        IMPLICIT NONE
        REAL,INTENT(IN)::x
        func3=LOG(x)
        END FUNCTION
END MODULE

PROGRAM test
    USE test_function
    IMPLICIT NONE
    INTEGER::num
    REAL::x
    PROCEDURE(func1),POINTER::p
    WRITE(*,*)'Enter the number(1-3)'
    READ(*,*)num,x
    SELECTCASE(num)
        CASE(1)
            p=>func1
        CASE(2)
            p=>func2
        CASE(3)
            p=>func3
    ENDSELECT
    WRITE(*,*)p(x)
END PROGRA
```

过程指针在Fortran程序中非常有用，因为用户可以将一个**特定的过程**和**一个定义的数据类型关联起来**。

示例：

```fortran
MODULE mod_test
    IMPLICIT NONE
    !----------------------------------!
    !声明派生数据类型
    TYPE:: matrix(m,n)
    INTEGER,LEN:: m
    INTEGER,LEN:: n
    !声明一个m行n列的数组
    REAL,DIMENSION(m,n)::element
    !声明一个过程指针
    PROCEDURE(transpos),POINTER,PASS::p=>NULL()
    END TYPE
    !----------------------------------!
    CONTAINS
    !求其转置矩阵
    SUBROUTINE transpos(a,b)
        !matrix(*,*)声明方式:选择默认参数
        TYPE(matrix(*,*)),INTENT(IN)::a
        REAL,ALLOCATABLE,DIMENSION(:,:),INTENT(OUT)::b
        INTEGER,DIMENSION(2)::low,high
        INTEGER::i,j
        low=LBOUND(a%element)
        high=UBOUND(a%element)
        ALLOCATE(b(high(2),high(1)))
        DO i=low(1),high(1)
            DO j=low(2),high(2)
                b(j,i)=a%element(i,j)
            END DO
        END DO
    END SUBROUTINE
    END MODULE mod_test
    
PROGRAM test
    USE mod_test
    IMPLICIT NONE
    !接口
    INTEGER::i
    INTEGER,PARAMETER::m=3
    INTEGER,PARAMETER::n=11
    !首先声明一个3行3列的矩阵
    TYPE(matrix(m,n))::a
    REAL,ALLOCATABLE,DIMENSION(:,:)::b
    a%element=RESHAPE([(i,i=1,m*n)],[m,n])
    WRITE(*,*)a%element
    a%p => transpos
    CALL a%p(b)
     WRITE(*,*)b
    a%p => transpos2
    CALL a%p(b)
    WRITE(*,*)b
END PROGRAM test
```

注意，这和将过程与数据类型绑定不同，绑定是永久的，而由函数指针指向的过程可以在程序执行过程中发生变化。

**注意，参数化派生数据类型中声明过程指针时，引用的过程中如果包含该参数化派生数据类型，必须声明(*)默认值进行处理。**

```
All length type parameters of the passed object dummy argument must be assumed
```

## 14.9 二叉树结构



### 14.9.1 二叉树结构的重要性





### 14.9.2 构建二叉树结构







## 14.10 小结

### 14.10.1 遵循原则

1. 在程序单元中只要创建了指针，就将其赋为空或者给其赋值。这样会减少未定义分配状态引起的不明确性。
2. 在对大数组或派生数据类型进行排序或交换时，交换指向数据的指针而非数据本身将会更加高效。
3. 当内存回收时，一定要让所有指向该片内存的指针为空或者给这些指针重新赋值。这些指针中的一个会因为DEALLOCATE语句的使用而自动为空，但是其他的必须手动的使用NULLIFY语句或者指针赋值语句来让它们为空或重新指向其他内存。
4. 始终记得测试作为参数传递给过程的指针的状态。在大型程序中很容易出错，因为在大型程序中很可能会出现试图访问未关联的指针或者试图重分配己关联的指针（后者会造成内存泄漏）。

### 14.10.2 语法小结

1. **POINTER 属性**

   ```fortran
   !格式：
   Type,POINTER:: ptr1[,ptr2,...]
   !举例：
   INTEGER,POINTER::next＿value
   REAL,DIMENSION(:),POINTER::array
   ```

   POINTER属性在类型定义语句中声明变量为指针变量。

2. **POINTER语句**

   ```fortran
   !格式：
   POINTER:: ptr1[,ptr2,...]
   !举例：
   POINTER::p1,p2,p3
   ```

   POINTER声明列表中的变量是指针变量。**最好在类型定义语句中使用指针属性来声明指针而不是采用这种形式。**

3. TARGET属性

   ```fortran
   !格式：
   Type,TARGET:: varl[,var2,...]
   !举例：
   INTEGER,TARGET::num_values
   REAL,DIMENSION(:),TARGET::array
   ```

   TARGET属性在类型定义语句中声明了**合法的指针指向的目标变量**。

4. TARGET语句

   ```fortran
   !格式：
   TARGET:: varl[,var2,...]
   !举例：
   TARGET::my_data
   ```

   TARGET语句声明列表中的变量是指针指向的合法的目标变量。**最好在类型定义语句中使用TARGET属性来声明目标变量而不是采用这种形式。**