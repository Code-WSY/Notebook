# 13 高级O/I概念

## 13.1 更多格式描述符

Fortran所有格式符完整列表：

其中：c 表示**列数**(column)。d表示**小数点右边数字的个数**(decimal)。e 表示**指数的位数**。K表示**比例因子**（十进制小数点移动的位数）。m表示要显示的**最小数据位位数**(Minimum)。r表示**重复次数**(repeat)。w表示**字符域宽**(wide)。

|       格式       |            描述符            |                             用法                             |
| :--------------: | :--------------------------: | :----------------------------------------------------------: |
|                  |    **实型数据I/O描述符**     |                                                              |
|       Dw.d       |                              |                指数表示法中的双精度数(已废弃)                |
|       Ew.d       |            Ew.dEe            |                      指数表示法中的实数                      |
|      ENw.d       |           ENw.dEe            |                      工程表示法中的实数                      |
|      ESw.d       |           ESw.dEe            |                      科学表示法中的实数                      |
|       Fw.d       |                              |                     十进制表示法中的实数                     |
|                  |    **整型数据I/O描述符**     |                                                              |
|        Iw        |             Iw.m             |                      十进制格式中的整数                      |
|                  |   **实数或整数I/O 描述符**   |                                                              |
|        Bw        |             Bw.m             |                       二进制格式的数据                       |
|        Ow        |             Ow.m             |                       八进制格式的数据                       |
|        Zw        |             Zw.m             |                      十六进制格式的数据                      |
|                  |   **逻辑型数据I/O描述符**    |                                                              |
|        Lw        |                              |                           逻辑数据                           |
|                  |   **字符型数据I/O描述符**    |                                                              |
|        A         |              Aw              |                           字符数据                           |
|     'x...x'      |           nHx···x            |                           字符常数                           |
|     "x...x"      |           nHx···x            |                    (在Fortran95中已废弃）                    |
|                  |      **通用I/O描述符**       |                                                              |
|       Gw.d       |            Gw.dEe            |                任何类型都适用的通用编辑描述符                |
|        GO        |                              |          任何类型都适用的可调节宽度的通用编辑描述符          |
|                  |    **派生类型I/O描述符**     |                                                              |
| DT'string'(vals) |                              |                      派生类型编辑描述符                      |
|                  |        **舍入描述符**        |                                                              |
|        RU        |                              | 采用向上舍入的原则，为在当前I/O语句中在此描述符之后的所有描述符指定数值 |
|        RD        |                              | 采用向下舍入的原则，为在当前I/O语句中在此描述符之后的所有描述符指定数值 |
|        RZ        |                              | 采用向0舍入的原则，为在当前I/O语句中在此描述符之后的所有描述符指定数值 |
|        RN        |                              | 采用四舍五入的原则，为在当前I/O语句中在此描述符之后的所有描述符指定数值 |
|        RC        |                              | 采用兼容的舍入原则，为在当前I/O语句中在此描述符之后的所有描述符指定数值 |
|        RP        |                              | 采用处理器默认的舍入原则，为在当前I/O语句中在此描述符之后的所有描述符指定数值 |
|                  |       **十进制描述符**       |                                                              |
|        DC        |                              | 使用逗号作为分隔符，把当前I/O语句中所有在此描述符之后的描述符的十进制部分分隔开 |
|        DP        |                              | 使用点号作为分隔符， 把当前I/O语句中所有在此描述符之后的描述符的十进制部分分隔开 |
|                  |        **定位描述符**        |                                                              |
|        nX        |                              |                       水平距离：空n格                        |
|        /         |                              |                    垂直距离：向下移动一行                    |
|        Tc        |                              |                   TAB: 移动到当前行的第c列                   |
|       TLn        |                              |                   TAB: 向当前行左边移动n列                   |
|       TRn        |                              |                   TAB: 向当前行右边移动n列                   |
|                  |      **扫描控制描述符**      |                                                              |
|        :         |                              |                        格式扫描控制符                        |
|                  | **其他描述符（不常使用的）** |                                                              |
|        kP        |                              |             显示实数时的比例因子(已被ES、EN替换)             |
|        BN        |         为了兼容F66          |              Blank Null: 忽略数字输入域中的空格              |
|        BZ        |         为了兼容F66          |           Blank Zero: 将数字输入域中的空格解释为0            |
|        S         |                              |               Sign control: 使用系统默认的规则               |
|        SP        |                              |               Sign control: 在正数前显示'+'号                |
|        S         |                              |              Sign control: 在正数前不显示'+'号               |

一些说明：

1. **冒号(:)描述符**

   冒号描述符最常用于清晰地终止行中间无意义的输出。

   冒号描述符允许用户修改格式描述符的常见行为。冒号描述符在WRITE语句中就像条件停止点。如果需要输出较多的数值， 那么冒号**可被忽略**， 而按正常的格式执行格式化的WRITE 语句。但是，如果在格式化中有冒号，并且输出的数值不是很多，那么WRITE语句就会在**冒号处停止执行**。

   示例：

   ```fortran
   PROGRAM test
       IMPLICIT NONE
       REAL,DIMENSION(13)::a
       INTEGER::i
       a=[(13.- i,i=1,13)]
       WRITE(*,100)(i,a(i),i=1,13)
       100 FORMAT(/'The output values are: '/,&
                   4(2X,'X(',I2,')=',F5.2))
                   
   !WRITE语句变量d之前扫描到了格式的结尾，则程序丢弃当前的输入缓冲区，然后重新获取一个新的输入缓冲区(另起新行)，并在格式中最右边的开始括号处重新开始(带上前边的重复数字)输出。
   WRITE(*,110)(i,a(i),i=1,13)
       110 FORMAT('The new(:) output values are: '/,&
                   4(:,2X,'X(',I2,')=',F5.2))
   END PROGRAM
   !输出
   The output values are:
     X( 1)=12.00  X( 2)=11.00  X( 3)=10.00  X( 4)= 9.00
     X( 5)= 8.00  X( 6)= 7.00  X( 7)= 6.00  X( 8)= 5.00
     X( 9)= 4.00  X(10)= 3.00  X(11)= 2.00  X(12)= 1.00
     X(13)= 0.00  X(
   
   The new(:) output values are:
     X( 1)=12.00  X( 2)=11.00  X( 3)=10.00  X( 4)= 9.00
     X( 5)= 8.00  X( 6)= 7.00  X( 7)= 6.00  X( 8)= 5.00
     X( 9)= 4.00  X(10)= 3.00  X(11)= 2.00  X(12)= 1.00
     X(13)= 0.00
   ```

对于WRITE语句依旧要做补充说明：若输出变量大于个格式描述符，那么从最右边的一组括号处开始读取，带不带数字无所谓。

## 13.2 表式输入的默认值（,, 和 /）

表式(list-directed, 直接列表式）输入的优点是易于使用，因为不需要为它专门编写FORMAT语句。

一条有表式输入的READ语句对于从终端获取用户输入信息来说是非常有用的。用户可能在任何一列键入输入数据，READ语句仍能正确解析输入值。此外，有表式输入的READ语句还支持空值。如果输入数据行包含**两个连续的逗号**，那么输入列表中相应的值不变(设为默认值)。这一行为允许用户将一个或多个变量的数值默认设置为前面定义的数值。

```fortran
PROGRAM test_read
INTEGER :: i = 1, j = 2, k = 3
WRITE (*,*) 'Enter i, j, and k: '
READ (*, *) , i, j, k
WRITE (*,*) 'i, j, k = ', i, j, k
END PROGRAM test_read
!input
Enter i, j, and k:
1000,,-2002
!output
i, j, k = 1000 2 -2002
!同样可以使用斜线作为结束符而将一行中的其他值全部设置为默认值。
Enter i, j, and k:
1000 /
i, j, k = 1000 2 3
```

## 13.3 Fortran I/O语句详述

Fortran I/O语句：

|   语句    |                  功能                  |
| :-------: | :------------------------------------: |
|   OPEN    |     打开文件(将文件连接到I/O单元)      |
|   CLOSE   |  关闭文件（将文件与心单元的连接断开）  |
|  INQUIRE  |             检查文件的属性             |
|   READ    |        读取文件（通过I/O单元）         |
|   PRINT   |         向标准输出设备写入数据         |
|   WRITE   |     向文件写入数据（通过I/O单元）      |
|  REWIND   |     将文件指针移动到顺序文件开始处     |
| BACKSPACE | 将顺序文件中的文件指针向后移动一个记录 |
|  ENDFILE  |     将文件指针移动到顺序文件结束处     |
|  FLUSH*   | 将输出缓冲区的内容写入磁盘(不应再使用) |
|   WAIT*   |   等待异步I/O操作的完成(不应再使用)    |

## 13.4 I/O名称列表

I/O名称列表是一个输出**固定变量名**和**数值列表**的好方法，它也是**读入固定变量名**和**数值列表**的好方法。名称列表经常是作为整体来读写的变量名列表。

格式:

```fortran
NAMELIST /n1—grounp _name/varl[,var2,...]
```

- n1 _grounp name ：名称列表的名字

- varl, var2...：列表中的变量

- NAMELIST：说明语句必须出现在程序的第一条可执行语句之前。

  如果有多条具有同样名字的NAMELIST 语句，那么所有语句中的变量被连接起来，把它们视为一条大型语句来处理。通过使用面向名称列表的I/O语句可以读写NAMELIST中列出的所有变量。

### 13.4.1 面向名称列表的WRITE语句

面向名称列表的WRITE语句形式如下：

```fortran
WRITE(UNIT=unit,NML=nl_group—name,[... ])
```

当执行面向名称列表的WRITE语句时，名称列表中的所有变量名都会和其值一起按照特定的顺序输出出来。

输出的第一项是＆符号，其后跟随名称列表的名字，然后是一系列按照“NAME=value"格式的输出值,以'/'结束。

这些输出值可能出现在一行上，中间以逗号分开，也可以各自独立出现在一行，主要依赖特定处理器对名称列表的实现方法。

示例：说明直接的NAMELIST的WRITE语句

```fortran
PROGRAM write_namelist
IMPLICIT NONE
INTEGER:: i = 1, j = 2
REAL :: a = -999.,b = 0.
CHARACTER(len=12):: string ='Test string.'
NAMELIST / mylist / i,j,string,a,b
OPEN(8,FILE='output.nml',DELIM='APOSTROPHE')
WRITE (UNIT=8,NML=mylist)
CLOSE(8)
END PROGRAM write_namelist
,..
!OUTPUT.nml
&MYLIST
 I=1          ,
 J=2          ,
 STRING="Test string.",
 A= -999.000000    ,
 B=  0.00000000    ,
 /
```

### 13.4.2 面向名称列表的READ语句

面向名称列表的READ语旬通用形式如下：

```fortran
READ(UNIT=unit,NML=nl_group_name,[…]）
```

其中unit是将要从中读取数据的心单元，而nl_group_ name是将要读的名称列表的名字。

当执行面向名称列表的READ语句时，程序搜索带有＆nl_group_ name的输入文件，它表示名称列表的开始。然后读取名称列表中的所有数值，直到碰到字符'/'终止READ。

名称列表READ语句**不是必须给名称列表中的每个变量赋值**。

**<u>如果有些名称列表变量没有包含在输入文件列表中，那么在名称列表READ执行之后，这些变量值保持不变。</u>**

例如变量名称中有(NAMELIST/mylist /a,b,c)，输入文件中只有(a,b)，此时c变量保持不变。

面向名称列表的READ语句非常有用：

假设正在编写一个包含100个输入变量的程序。这些变量在程序中初始化为常用的默认值。

在程序的某一特殊执行过程中，如需要改变这些变量前10个初始值，其他变量值保待不变。这种情况下，就可以将100个变量**放在名称列表中**，并在程序中包括面向NAMELIST的READ语句。

当用户运行程序时，他只要在名称列表输入文件中给出少量几个值，其他的输入变量会保持不变。

这一方法要比使用普通的READ语句好很多，因为在普通的READ输入文件中要列出所有100个变量，即使在特殊过程中它们的值不需要改变。

示例：说明直接地NAMELIST的READ语句

```fortran
PROGRAM read_namelist
IMPLICIT NONE
!原始数据
INTEGER :: i=2,j=3
REAL :: a=-100.,b=0.
CHARACTER(len=12) :: string = 'Test string.'
!需要更新的数据
NAMELIST /myList/i,j,string,a,b
!打开最新数据文件
OPEN(UNIT=100,FILE='output.nml',DELIM='APOSTROPHE')
WRITE(*,'(A)')'Namelist file before update: '
!展示原始的数据
WRITE (UNIT=*,NML=myList)
!从最新文件中更新数据，并改写变量值
READ(UNIT=100,NML=myList)
!READ (UNIT=7,NML=mylist)
!展示更新之后的数据
WRITE (*,'(A)') 'Namelist file after update: '
WRITE (UNIT=*,NML=myList)

END PROGRAM read_namelist
```

**注意**：

output.nml有时候会在使用READ()时，读取报错：Fortran runtime error: End of file。

解决办法：**紧靠最后一个赋值末尾加上‘/’,如果没有用，另起一行再加‘/’**

------

使用NAMELIST I/O保存数据，以便在程序之间或者程序的不同运行之间**交换数据**。同样，可以使用NAMELIST READ语句在程序执行时**更新选中的输入参数**。

数组名，部分数组以及数组元素都可以出现在NAMLIST 语句中。如果数组名出现在名称列表中，那么当执行名称列表WRITE 时，数组的每个元素都会一次输出到输出名称列表中， 例如a (1) =3., a (2) =-1等。当执行名称列表READ 时，可以单独给每个数组元素赋值，输入文件中只需列出要改变值的数组元素。
**形参**和**动态创建的变量**不能出现在NAMELIST 中，这包括**无上下界值数组的形参**、**长度不定的字符变量**、**自动变量以及指针**。

如果打开名称列表输出文件时使用的是'APOSTROPHE'或'QUOTE'作为**定界符**， 那么由名称列表WRITE语句所写入的输出文件是可以被名称列表READ语句直接读取的。

这一事实给相互独立的程序之间或者同一程序不同运行之间大量数据的相互交换带来了极大的方便。

## 13.5 未格式化文件

### 13.5.1 格式化文件

**定义**：由可识别的字符、数字等组成，按照标准编码格式存储，如ASCII码或EDCDIC码。这些文件易于区别，因为当在屏幕上显示或将文件使用打印机打印出来时，文件中所有的字符和数字都是可读的。

**优点**：可直接识读其内容是何种数据。

**缺点**：因为处理器内部表示和文件中字符的不同，处理器在转换数据时必须做大量的工作。在格式化时可能导致截尾或舍入错误。

### 13.5.2 未格式化文件

**定义：**一般为传统二进制文件。

**缺点：**不能直接被人类所识别和解释，而且它通常不能在不同型号的处理器之间移动，因为不同型号的处理器其内部表达整数和实数的方法也不同。

**优点：**直接将处理器内存的信息复制到磁盘，而不需要任何转换的方式。不会浪费时间格式化数据，而且数据所占用的磁盘也要小很多。不存在截尾或舍入错误

### 13.5.3 未格式化I/O语句(form='unformatted')

除了READ和WRITE语句中的控制列表中不出现FMT语句之外，未格式化I/O语句看上去和格式化I/O语句类似。

示例：

```fortran
PROGRAM NAMELISTFILE
    INTEGER,DIMENSION(1000)::arr=[(i,i=1,1000)],B
    CHARACTER(LEN=100)::a,a2
!创建一个格式化文件unit10
OPEN(UNIT=10,FILE='GSH')
!写入arr数据
WRITE(UNIT=10,FMT=100,IOSTAT=istat)(arr(i),i=1,1000)
!五个数据一行
100 FORMAT(5I13)
CLOSE(10)

!创建未格式化文件（form='unformatted'）unit11
OPEN(UNIT=11,FILE='WGSH',form='unformatted')
!将其格式化文件之后转化存入GSH2格式化文件中unit12
OPEN(UNIT=12,FILE='GSH2')
!数据写入未格式化文件中
WRITE(UNIT=11,IOSTAT=istat,iomsg=a)(arr(i),i=1,1000)
!注意 写入之后文件定位在文件末尾，读取时需要回到开头。
REWIND(11)
!读取格式化数据
READ(11,IOSTAT=istat2,iomsg=a2)(B(1000-i),i=0,999)
!打印到屏幕上
WRITE(*,110)(B(i),i=1,1000)
!存入格式化文件中
WRITE(12,110)(B(i),i=1,1000)
110 FORMAT(3I10)
CLOSE(11)
END PROGRAM
```

一个文件可以是格式化的， 也可以是未格式化的， 但不能两者兼顾。因此，不能将格式化I/O语句和未格式化I/O语句**混用在同一个文件中**。

INQUIRE语句可以用来判定文件的格式化状态。

使用格式化文件创建人类**必须可读的文件**，或者**必须在不同的处理器之间传输的文件**。使用未格式化文件来有效的存储大量不需要直接识别，并且始终保存在同一型号处理器上的数据。

同样，当I/O速度非常关键的时候， 使用未格式化文件。

## 13.6 直接访问文件

直接访问文件是指使用**直接访问模式读写的文件**。直接访问的、未格式化文件可能是计算机上**最高效**的Fortran 文件。

顺序访问文件中的记录必须按照顺序从第一条记录访问到最后一条。相反， 直接访问文件中的记录可以按照任意顺序来访问。直接访问文件**对于需要以任意顺序读取的信息非常有用**，比如数据库文件。

直接访问文件中的每条记录的长度必须相等。。如果记录等长，那么精确的计算磁盘文件中第i条记录的位置就非常简单，这样就可以直接读取含有该条记i录的磁盘扇区，而无需读取该记录前的其他所有扇区。

例如， 假设想要读取一个每记录长度为100个字节的直接访问文件中的第120条记录。那么第120 条记录就位于文件中字节11901到12000之间。计算机可以根据计算出含有那些字节的磁盘扇区，直接读取之。

**直接访问格式化文件OPEN语句格式**：

```fortran
OPEN(UNIT=8, FILE='dirio.fmt', ACCESS='DIRECT', FORM='FORMATTED',RECL=40)
```

OPEN语句中指定**ACCESS='DIRECT'**的方法来打开。

OPEN语句中指定RECL=number来指明每个记录的长度。

对于格式化文件来说，RECL＝子句中每个记录的长度是按字符单位计算的。

因此，上述文件dirio.fint中每条记录的长度都是40个字符。

对未格式化文件来说， RECL＝子句中指明的长度可以按照**字节**、**字**或者**其他机器相关的度量单位**来表示。

可以使用**INQUIRE**句来确定与处理器无关情况下的未格式化直接访问文件中每条记录的长度。

**直接访问格式化文件READ语句格式**：

```fortran
READ(8,'(I6)',REC=irec)ival
```

直接访问文件的READ 和WRITE 语句看上去和顺序访问文件的相同，除了它包含了用于指定要读写的**特定记录**的REC之外（如果省略了REC，那么读写的记录就为**文件的下一条记录**）

示例：说明如何直接访问Fortran文件

```fortran
!格式化文件的直接访问
PROGRAM direct_file
    IMPLICIT NONE
    INTEGER::i,irec
    CHARACTER(LEN=20)::line
    OPEN(UNIT=8,FILE='direct.file',ACCESS='direct',FORM='formatted',RECL=40)
    DO i=1,1000
        WRITE(8,'(A,I4,A)',rec=(1002-i))'The recod is ',i,'.'
    END DO
    !ADVANCE 就是输入语句之后是否换行，YES是不换行。
        WRITE(*,'(A)',ADVANCE='YES')'Which one are you looking for ?'
        READ(*,*)irec
    READ(8,'(A)',rec=irec) line
    WRITE(*,*)line
END PROGRAM

!未格式化文件的直接访问
PROGRAM direct_file
    IMPLICIT NONE
    INTEGER::i,irec,line
    !CHARACTER(LEN=20)::line
    OPEN(UNIT=8,FILE='direct.txt',ACCESS='direct',FORM='unformatted',RECL=40)
    DO i=1,1000
        WRITE(8,rec=(1001-i))i
    END DO
        WRITE(*,'(A)',ADVANCE='YES')'Which one are you looking for ?'
        READ(*,*)irec
    READ(8,rec=irec) line
    WRITE(*,*)line
END PROGRAM
```

## 13.7 流访问模式*

流访问模式**按照字节读写文件**，并且**不处理其中的特殊字符，比如回车、换行等**。

这点和顺序访问方式不同，顺序访问方式是按记录读写数据，使用回车和/或换行作为当前处理的记录的结束标志。同样使用write语句写入数据，但一条write语句结束后并不换行。**要插入换行符（类似于C语言的\n），需要用换行命令newline()。**

流访问模式和C语言中的I/O函数getc, putc 功能类似， 每次读写一个字节，文件中的控制字符和其他字符的处理方式相同。

OPEN语句中通过使用ACCESS='STREAM'就可以按照流访问方式打开文件。

格式：

```fortran
OPEN (UNIT=8,FILE='infile.dat', ACCESS='STREAM', FORM='FORMATTED',IOSTAT=istat)
```

示例：

```fortran
PROGRAM reader
 INTEGER::a(1024),i,b(1024)
 OPEN(FILE="test",UNIT=8,ACCESS="stream",FORM="formatted")
 b=[(i,i=1,1024)]
 DO i = 1,1024
    WRITE(8,'(I4)')b(i)
    WRITE(8,'(A)')NEW_LINE('caaaa')
 ENDDO
 CLOSE(8)

 OPEN(FILE="test",UNIT=8,ACCESS="stream",FORM="formatted")
  DO i=1,1024
    READ(8,'(I4)')a(i)
 ENDDO
   DO i = 1,1024
    WRITE(*,100)a(i)
 ENDDO
100 FORMAT(3I4)
END
```

流访问模式在测试的时候发现对于格式化文件似乎并没有很好的体现其功能。不知道是不是我在对其功能的理解上有问题。

**流 I/O 在与C语言·程序创建或读取的文件进行互操作时非常有用**

对于需要按照顺序读取和处理的数据使用顺序访问文件。对于可以按任意顺序读写的数据使用直接访问文件。

对于需要快速处理大量数据的应用程序， 使用直接未格式化文件。如果可能， 让文件中的记录长度为当前机器基本磁盘扇区大小的倍数。

## 13.8 派生数据类型的非默认I/O

已经知道默认情况下，派生数据类型是按照其在类型定义语句中定义的顺序读写的，并且Fortran描述符的顺序必须和派生数据类型中各元素的顺序一致。

可以创建一个非默认用户自定义的方式来读写派生数据类型数据。只要**将过程和要进行输入输出处理的数据类型绑定即可**。一共有四种类型的过程可用，分别是格式化输入、格式化输出、未格式化输入以及未格式化输出。可以用如下所示的方法声明一个或者多个过程，并将其和数据类型绑定。

```fortran
TYPE :: point
REAL :: x
REAL :: y
CONTAINS
GENERIC :: READ(FORMATTED) => read_fmt
GENERIC :: READ(UNFORMATTED)=> read_unfmt
GENERIC :: WRITE(FORMTTED) => write_fmt
GENERIC :: WRITE(UNFORMATTED) => write_unfmt
END TYPE
```

对于其他类型的I/O操作来说，可以通过调用在通用READ (FORMATTED)行上指定的过程，完成格式化读出等。

以通过在I/O语句中指定DT格式化描述符来访问绑定的过程。

格式：

```fortran
DT 'string' (10,-4,2)
```

其中字符串和参数列表传递给完成I/O功能的过程。字符串是可选的，对于某些用户自定义的I/O操作，如果不需要可以删除它。

完成I/O功能的过程必须有如下接口：

```fortran
SUBROUTINE formatted_io(dtv,unit,iotype,v_list,iostat,iomsg)
SUBROUTINE unformatted_io(dtv,unit,iostat,iomsg)
```

参数说明：

1. dtv是要读写的派生数据类型。

   对千WRITE 语句来说，必须使用INTENT (IN)来声明此值，并且不能修改。对于READ 语句来说，必须使用INTENT (INTOUT) 声明此值，并且读入的数据必须存入此变量中。

2. unit 是要读/写的I/O单元编号。必须使用INTENT (IN) 语句将其声明为整型。

3. iotype是INTENT(IN)的CHARACTER(len=*)变量。包含下列三个可能的字符串之一：

   - 'LISTDIRECTED'：针对表式I/O操作。

   - 'NAMELIST'：针对I/O名称列表操作。
   - 'DT'//string, 普通格式化I/O操作，其中string是DT 格式描述符中的字符串。

4. v_list声明为INTENT (IN) 的整型数组，该数组中整数集是放在DT格式描述符的圆括号中。

5. iostat是I/O状态变量， 在执行完过程后对该状态变量进行设置。

6. iomsg 是声明为INTENT (OUT) 的CHARACTER (len=＊)变量。如果iostat 非零，那么就必须将此变量的值赋为一条消息。否则，该变量值不会改变。

每个子例程都按照程序员期望的方式执行特定类型和方向的I/O操作。只要接口明确了，那么非默认的I/O可以和其他Fortran I/O特性无缝连接完成。

## 13.9 异步I/O

### 13.9.1 异步操作简介

Fortran 2003和后期版本定义了一种新的I/O模式——异步I/O。

在普通的FortranI/O操作中，如果程序使用WRITE语句向文件写入数据，那么程序执行到WRITE语句处就会暂停执行，直到数据全部写完，才会继续执行。同样，如果程序使用READ语句从文件中读数据，那么程序遇到READ时就会停止，直到数据全部读完才会继续执行。这就是同步I/O, 因为I/O操作和程序的执行是同时的。

相反，异步**I/O操作**和**程序的执行**是**并行执行**的。

如果执行异步WRTTE语句，那么待写入文件的数据会先拷贝到一些内部缓冲区，当写过程启动后，控制马上返回到调用程序。这种情况下，调用程序可以继续全速执行，而写操作也会同时执行。

对千异步**READ**来说，情况会更加复杂一些。如果执行的是异步READ语句，那么在启动读过程后，控制立即返回到调用程序。当执行返回的调用程序时，**被读的变量是未定义的**。它们的值可能是旧值，也可能是新值， 还可能正在更新，所以**在读操作完成前， 这些变量不能使用**。**计算机可以继续执行，完成其他计算，但是不能使用异步READ语句中的变量，除非读操作完成。**

程序获取异步读操作的状态：

1. 当启动I/O操作时，首先使用**ID=子句**为该操作获取一个ID,然后使用**INQURIE**语句查询操作的状态。

2. 让程序执行**WAIT**指令或者在相应的I/O单元上使用文件定位语句(REWIND, BACKSPACE)。

在这两种情况下，直到所有该单元的所有I/O操作完成后，控制才会立即返回到调用程序，这样程序就能在执行恢复之后安全地使用新数据了。

使用异步I/O的典型方法:

1. 启动读操作
2. 再执行一些其他的运算
3. 调用WAIT语句确保在使用读回的数据之前完成该I/O读操作

如果程序结构合理， 应该能够保证绝大多数时间里程序都是执行状态而非阻塞状态。并且大型并行计算机总是支持异步I/O操作。

### 13.9.2 执行异步I/O

为了使用异步I/O操作， 首次打开文件时必须使用允许异步I/O的选项， 并且每个单独的READ和WRITE语句都必须选择支持异步I/O选项。

如果执行异步WRITE, 那么程序不需要采取其他专门的操作。

如果执行异步READ, 那么程序必须等待READ执行完毕后才能使用其中的变量。

1. 异步WRITE操作

   ```fortran
   REAL,DIMENSION (5000, 5000) :: data1
   OPEN(UNIT=B,FILE='x.dat',ASYNCHRONOUS='yes',&
   STATUS='NEW',ACTION='WRITE',IOSTAT=istat)
   !输出数据到文件
   WRITE(8,1000,ASYNCHRONOUS='yes',IOSTAT=istat) datal
   1000 FORMAT (10F10.6)
   !（继续处理……）
   ```

   注意，关键词ASYCHRONOUS必须在OPEN语句和WRITE语句中都出现。

2. 异步READ操作

   ```fortran
   REAL,DIMENSION (5000, 5000) :: data2
   OPEN(UNIT=B,FILE='x.dat',ASYNCHRONOUS='yes',&
   STATUS='NEW',ACTION='WRITE',IOSTAT=istat)
   !输出数据到文件
   READ(8,1000,ASYNCHRONOUS='yes',IOSTAT=istat) data2
   1000 FORMAT (10F10.6)
   !（继续处理……）
   WAIT(8)
   !(x)
   ```


### 13.9.3 异步I/O的问题

当Fortran编译器试图优化执行速度时，容易出现异步I/O 操作问题。现代的优化型编译器通常会改变动作的顺序，并行执行操作，以便提高程序整个执行的速度。

通常情况下，这样做程序能够很好地运行。但是，如果编译器将一条语句移动到了I/O单元的WAIT语句之前，并且被移动的语句使用了异步READ中的数据，那么优化型编译器会带来问题。这种情况下，所使用的数据可能是旧信息，也可能是新信息，还可能是两者的组合！

Fortran 定义了一个新的属性，可以针对异步I/O的此类问题向编译器提出警告。
ASYNCHRONOUS 属性或者语句规定了这种警告的格式：

```fortran
REAL,DIMENSION(1000),ASYNCHRONOUS::datal
ASYNCHRONOUS::x,y,z
```

当某变量（或变量成分）出现在与异步I/O语句相关的输入输出列表或名称列表时，**ASYNCHRONOUS 属性会自动赋给变量**。在这种情况下不需要用ASYNCHRONOUS 属性声明变量，所以实际效果中，通常在变量的声明语句中看不到显式的异步声明。

## 13.10 访问特定处理器相关的I/O系统信息

Fortran 包括一个内置模块，该模块提供了一个不依赖于处理器的获取处理器I/O系统信息的方法。这个模块就是ISO_FORTRAN_ENV。

作用：使用这些常量而不是将对应的值进行硬编码，那么程序的可移植性会更高。如果程序移植到另外一个处理器上，那么该处理器上ISO_FORTRAN_ENV 所包含的值就是新环境下的修正值，而代码本身不需要修改。

示例：

```fortran
USE ISO_FORTRAN_ENV
WRITE(OUTPUT_UNIT,*)'This is a test'
```

| 常量                    | 值/描述                                                      |
| ----------------------- | ------------------------------------------------------------ |
| INPUT_UNIT              | 整型，含标准输入流单元编号，常使用READ(\*,\*)语句访问该单元  |
| OUTPUT_UNIT             | 整型，含标准输出流单元编号，常使用WRITE (*,＊)语句访问该单元 |
| ERROR_UNIT              | 整型， 包含标准错误流的单元编号                              |
| IOSTAT_END              | 整型，包含当到达文件结尾时，IOSTAT中的READ语句的返回值       |
| IOSTAT_EOR              | 整型，包含当记录结束时，IOSTAT中的READ语句的返回值           |
| NUMERIC_STORAGE_SIZE    | 整型，包含默认数字型数值的比特数                             |
| CHARACTER_SSTORAGE_SIZE | 整型，包含默认字符型数值的比特数                             |
| FILE_STORAGE_SIZE       | 整型，包含默认文件存储单元的比特数                           |

## 13.11 小结

### 13.11.1 遵循原则

1. 在新程序中绝不要使用D, P, BN, BZ, S, SP 或SS 格式描述符。
2. 不要在Fortran 程序中依赖预先连接的文件（标准输入输出文件除外）。预先连接文件的编号和名字随着处理器的不同而不同，所以使用它们会降低程序的可移植性。相反，应该始终使用OPEN语句显式打开每个要用的文件。
3. 在OPEN语句中始终使用IOSTAT和IOMSG来追踪错误。当检测到错误时，关闭程序之前告诉用户所发生的问题或者让用户选择使用一个其他文件。
4. 在程序使用完文件后，尽可能快地使用CLOSE 语句显示关闭每个磁盘文件，这样在**多任务环境中**，其他人也能使用该文件。
5. 检查输出文件是否覆盖了一个已存在文件。如果是，确认用户确实想覆盖文件中原来的数据。
6. 在READ语句中使用IOSTAT和IOMSG来防止程序在发生错误、遇到文件结束条件或者遇到记录结束条件时意外终止。当发生错误或者碰到文件结束条件时，程序可以采用恰当的方法继续执行或者合理地关闭。
7. 使用NAMELIST I/O来存储数据，以便在程序之间或者同一个程序的不同运行之间交换数据。同样，也可以使用NAMELIST READ语句更新程序执行时所选择的输入参数。
8. 使用格式化文件创建人类可读的文件，或者便于在不同型号的机器之间进行移植的文件。使用未格式化文件可以有效地存储大量的不需要直接检查的数据，并且这些数据只保存在一种类型的计算机上。同样，当I/O速度至关重要时使用未格式化文件。
9. 对于需要顺序读取和处理的数据使用顺序访问文件。对于必须按照任意顺序读写的数据使用直接访问文件。
10. 对于需要快速操控大量数据的应用程序，使用直接访问的未格式化文件。如果可能，让文件中的记录长度为所在计算机的基本磁盘扇区的整数倍。

## 13.11.2 语法小结

1. **BACKSPACE语句**

   ```fortran
   !格式：
   BACKSPACE(control_list)
   !或者 BACKSPACE (unit）
   !或者 BACKSPACE unit
   !例子
   BACKSPACE(10,IOSTAT=istat)
   BACKSPACE(8)
   ```

   BACKSPACE语句将当前文件指针后移一条记录，control_list 中能出现的子句有UNIT,IOSTAT和ERR.
   
2. **ENDFILE语句**

   ```fortran
   !格式:
   ENDFILE(control_list)
   !或ENDFILE(unit）
   !或ENDFILE unit
   !例子：
   ENDFILE(UNIT=10，IOSTAT=istat)
   ENDFILE(8)
   ```

   ENDFILE语句**将文件结束符记录写入文件**，并且将文件**指针**定位到文件结束记录之后。control_list中能出现的子句有UNIT, IOSTAT和ERR。

3. **FLUSH语句**

   ```fortran
   !格式：
   FLUSH(control_list)
   !例子：
   FLUSH(8)
   ```

   将内存缓冲区中的输出数据强制写入磁盘。

4. **INQUIRE语句**

   ```fortran
   !格式：
   INQUIRE{control_list)
   !例子：
   LOGICAL::lnamed
   CHARACTER(len= 12)::filename,access
   INQUIRE(UNIT=22,NAMED=lnamed,NAME=filename,ACCESS=access)
   ```

   INQUIRE语句允许用户**判定文件属性**。可以通过指定的文件名或者（在文件打开后）文件对应的心单元编号来指明文件。INQUIRE语句中可用的子句在表l4-5中列出。

5. **NAMELIST语句**

   ```fortran
   !格式：
   NAMELIST/nl_group_name/varl[,var2,...]
   !例子：
   NAMELIST/control_list/page_size,rows,colums
   WRITE(8,NML=control_list)
   ```

   NAMELIST语句是将一组变量和一个名称列表关联的规格说明语句。使用针对名称列表的WRITE和READ语句，把名称列表中所有变量作为一个单元进行读写。当读名称列表时，只有出现在输入列表中的变量可以由READ语句修改。输入列表中的变量以关键字格式出现，并且各个变量的顺序任意。

6. **PRINT语句**

   ```fortran
   !格式：
   PRINT fmt,output_list
   !例子：
   PRINT *,intercept
   PRINT '2I6' ,i,j
   ```

   PRINT语句将输出列表中的数据按照格式描述符中所指定的格式输出到标准输出设备中。格式描述符可以写在FORMAT语句中，也可以是一个字符串或者是带有星号的表控I/O默认的格式。

7. **REWIND语句**

   ```fortran
   !格式：
   REWIND(control_list)
   !例子：
   REWIND (10)
   REWIND 12
   ```

   REWIND语句将当前文件**指针**移动到文件的开始处，control_list中可用的子句包括：UNIT，IOSTAT和ERR。

8. **WAIT语句**

   ```fortran
   !格式：
   WAIT(control_list)
   !例子：
   WAIT(8)
   ```

   WAIT语句等待悬挂的异步I/O操作完成后，才返回调用程序。
