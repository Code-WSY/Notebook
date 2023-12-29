# 4 基本的I/O概念



## 4.1 格式和格式化WRITE语句 

格式可用来指定程序打印输出变量的确切方式。通常来说，格式可以指定变量在纸上的水平和垂直位置，以及要打印输出的有效位数。用于整数**i**和实数变量**result**的典型格式化WRITE语句如下所示：

```fortran
WRITE(*,100)i,result
100 FORMAT('The result for iteration ',I3,' is ',F7.3)
```

- FORMAT语句包含WRITE语句所使用的格式化信息。
- 出现在**WRITE**语句括号内部的数字**100**是用于描述如何打印输出**i**和**result**数值的FORMAT语句的**语句标号**。
- **I3**和**F7.3**分别是变量**i**和**result**相关的格式描述。

## 4.2 输出设备

Fortran 控制字符(目前已无效)：

| 控制字符 |           作用           |
| -------- | :----------------------: |
| 1        |         跳到新页         |
| 空格     |         单行间距         |
| 0        |         双行间距         |
| +        | 没有行距(在前一行上打印) |

```fortran
WRITE(*,100) 
100 FORMAT('l','This heading is at the top of a new page.') 
WRITE(*, 110) 
110 FORMAT('0',' Control Character Action ') 
WRITE(*,120) 
120 FORMAT(' ',' ================= ====== ')
```

控制字符是专门用于行式打印机的特殊机制。行式打印机实际上已经过时多年， 所以将第1列作为控制字符的用法已经从Fortran2003标准中删除了。 按照新的标准，输出缓冲区的第一列是一个没有特殊用途的普通字符。它像任何其它字符一样可以被打印出来。

## 4.3 格式描述符

有许多不同的格式描述符。 它们分为四个基本的类别： 

1. 描述文本行**垂直位置**的格式描述符。
2. 描述行中数据**水平位置**的格式描述符。
3. 描述特定数值的**输出格式**的格式描述符。
4. 控制格式中一部分的重复的格式描述符。

| 符号 | 含义                                 |
| :--: | :----------------------------------- |
|  c   | 列号                                 |
|  d   | 实数输入或输出的**小数位右边的位数** |
|  m   | 要显示的**最小位数**                 |
|  n   | 要跳过的**空格数**                   |
|  r   | 重复计数，一个或一组描述符的使用次数 |
|  w   | 域宽，输入或输出占用的字符数         |

| 描述符 |               含义                |     一般格式      |
| :----: | :-------------------------------: | :---------------: |
|   I    |             整数输出              |    rIw、rIw.m     |
|   F    |             实数输出              |       rFw.d       |
|   E    |    实数输出，指数表示(0.1~1e)     | rEw.d (w$\ge$d+7) |
|   ES   |       真正的科学计数(1~10e)       | rESw.d(w$\ge$d+7) |
|   L    |             逻辑输出              |        rLw        |
|   A    |             字符输出              |      rA、rAw      |
|   X    |       水平定位(插入空格数n)       |        nX         |
|   T    | 水平定位(跳入指定列c,可能会重叠)  |        Tc         |
|   /    | 改变输出行(一般结合&进行代码分行) |        /,&        |

```fortran
CHARACTER(len=10)::first_name = 'James'
CHARACTER::initial = 'R'
CHARACTER(len=16)::last_name = 'Johnson'
CHARACTER(len=9):: class1 = 'COSC 2301'
INTEGER::grade = 92
WRITE(*,100)first_name,inital,last_name,grade,class1
100 FORMAT(A10,1X,A1,1X,A10,4X,I3,T50,A9)
```

格式描述符组的重复使用：

```fortran
300 FORMAT(I6,I6,F10.2,F10.2,I6,F10.2,F10.2)
=
300 FIRNAT(I6,2(I6,F10.2,F10.2))
```

```fortran
310 FORMAT(I6,F10.2,A,F10.2,A,I6,F10.2,A,F10.2,A)
=
310 FORMAT(2(I6,2(F10.2,A)))
```

如果用一个星号来代替重复次数的话，只要还有待打印输出的数据，则括号内的内容将无限次重复使用，直到没有可输出的数据为止。

```fortran
320 FORMAT(I6,*(I6,2F10.2))
```

## 4.4 格式化READ语句

| 描述符 |                  含义                  | 一般格式 |
| :----: | :------------------------------------: | :------: |
|   I    |                整数输入                |   rIw    |
|   F    |                实数输入                |  rFw.d   |
|   L    |                逻辑输入                |   rLw    |
|   A    |                字符输入                | rA、rAw  |
|   X    | 水平定位(跳过输入数据中不想读取的区域) |    nX    |
|   T    | 水平定位(跳过输入数据中不想读取的区域) |    Tc    |
|   /    |                垂直定位                |   /,&    |

例：

- ```fortran
  READ(*,100)increment
  100 FORMAT(6X,I6)
  !跳过缓冲区的前六列，内容7~12列将被解释为证书，存入increment
  ```

- ```fortran
  READ(*,'(3F10.4)')a,b,c
  !假设输入：1.5    0.15e+01   15e-01
  !都将被计入为1.5
  ```

  ```fortran
  !如果在区域中出现的是不带小数点的数值，那么则假定小数点出现在由格式描述符中的d项指定的位置上
  !如果格式描述符为F10.4，则输入数值的最右边四位就为小数部分，其余位数为整数部分
  READ(*,'(3F10.4)')a,b,c
  !假设输入：1.5    15  15000
  !计入
  !a=1.5000 b=0.0015 c=1.5000 
  ```

  <u>**因此，在使用格式化READ语句时，总是使用带小数点的实数。**</u>

- ```fortran
  CHARACTER(len=10)::string_1,string_2
  CHARACTER(len=5)::string_3
  CHARACTER(len=15)::string_4,string_5
  READ(*,"(A)")string_1
  READ(*,"(A10)")string_2
  READ(*,"(A10)")string_3
  READ(*,"(A10)")string_4
  READ(*,"(A)")string_5
  WRITE(*,"(A)")string_1
  WRITE(*,"(A10)")string_2
  WRITE(*,"(A10)")string_3
  WRITE(*,"(A10)")string_4
  WRITE(*,"(A)")string_5
  !假设输入数据为：
  123456789012345
  123456789012345
  123456789012345
  123456789012345
  123456789012345
  !则赋值为
  string_1=1234567890
  string_2=1234567890
  !string_3只有5个字符长(数值)，A10描述符是10个字符长(域宽)，因此后五个字符代替了前5个
  string_3=     67890
  string_4=1234567890     
  string_5=123456789012345
  ```

**rA**: 描述读取一个区域中的字符数据，区域宽度与被读取的字符变量长度相同。

**rAw**:描述符在一个固定宽度**w**的区域中读取字符数据，如果区域宽度大于字符变量长度，则将该区域最右边部分的数据读入到字符变量中。如果小于字符变量长度，则将该区域中的字符保存到变量的最左边，变量的其余部分用空格填充。

- ```fortran
  !读取缓冲区的第1至6个字符中的数值，一次读为整数，一次读为字符串。
  CHARACTER(len=6)::string
  INTEGER::input
  READ(*,"(I6,T1,A6)")input,string
  !输入：
  123456789
  !读入：
  input=123456
  string='123456'
  ```

- ```fortran
  REAL::a,b,c,d
  READ(*,300)a,b,c,d
  300 FORMAT(2F10.2,//,2F10.2)
  !输入：
  1.0   2.0   3.0
  4.0   5.0   6.0
  7.0   8.0   9.0
  !读入：
  a=1.0  b=2.0  c=7.0  d=8.0
  ```

注意：

1. 如果READ语句在格式结束前用完了所有变量，则格式的使用就停在最后堆区的变量后面。下一个READ语句将从一个新的输入缓冲区开始，原有的输入缓冲区中的其他数据被丢弃。

   - ```fortran
     READ(*,30)i,j
     READ(*,30)k,l,m
     30 FORMAT(5I5)
     !输入：
     1 2 3 4 5
     6 7 8 9 0
     !读入
     i=1 j=2 k=6 l=7 m=8
     ```

2. 如果在READ语句变量全赋完值之前扫描到了格式的结尾，则程序丢弃当前的输入缓冲区，然后重新获取一个新的输入缓冲区，并在格式中**最右边不带重复次数**的**开始括号处**重新开始，WRITE一样。

   - ```fortran
     READ(*,40)i,j,k,l,m
     40 FORMAT(I2,(T5,2I2))
     !输入：
     1 2 3 4 5
     6 7 8 9 10
     !读入:
     i=1 j=3 k=4
     l=8 m=9
     
     ```

     

## 4.5 文件及文件处理介绍

Fortran 文件控制语句：

|  I/O语句  |                 作用                  |
| :-------: | :-----------------------------------: |
|   OPEN    | 将特定磁盘文件与特定的i/o单元号相关联 |
|   CLOSE   | 结束特定磁盘文件与特定i/o单元号的关联 |
|   READ    |        从特定i/o单元号读取数据        |
|   WRITE   |        向特定i/o单元号写入数据        |
|  REWIND   |          移动到文件夹的开头           |
| BACKSPACE |       在文件中向前移动一条记录        |

### 4.5.1 OPEN语句

 OPEN语句将一个文件与一个给定的i/o单元号关联起来。格式为：

```fortran
OPEN(open_list)
```

其中open_list包含一组子句，分别指定i/o单元号、文件名和关于如何存取文件的信息，列表中的子句用逗号隔开。

最重要的六项子句：

| 子句项 | 作用                      | 格式                                                         |
| :----: | ------------------------- | ------------------------------------------------------------ |
|  UNIT  | 指明与文件关联的i/o单元号 | UNIT=int_expr(非负整数)                                      |
|  FILE  | 指定要打开的文件名        | FILE=char_expr                                               |
| STATUS | 指定要打开文件的状态      | STATUS=char_expr (OLD,NEW,REPLACE,SCRATCH,**UNKNOW**)        |
| ACTION | 指定一个文件打开方式      | ACTION=char_expr (READ,WRITE,**READWRITE**)                  |
| IOSTAT | 指定一个整数变量          | IOSTAT=int_var (OPEN执行成功，赋值变量为0，否则返回错误相应正数值) |
| IOMSG  | 指定一个字符变量名        | IOMSG=chart_var(OPEN执行成功，字符变量内容不变，否则返回错误信息) |

new: 文件原先不存在,如果打开时文件已存在，则会报错。
old:   文件已经存在, 如果打开时文件不存在，则会报错。
replace: 如果文件已经存在则会覆盖老文件, 如果不存在则新建。
scratch: 文件会被自动命名, 系统将会打开一个**暂存盘**, 程序运行结束后文件消失。
unknown: 不同编译器不同, 一般为replace


示例：

1. **打开文件进行输入**

   ```fortran
   INTEGER::ierror
   OPEN(UNIT=8,FILE='example.dat',STATUS='OLD',ACTION='READ',IOSTAT=ierror,IOMSG=err_string)
   ```

2. **打开文件进行输出**

   ```fortran
   INTEGER::ierror,unit
   CHARACTER::err_string,filename
   unit=25
   filename='OUTCAR'
   OPEN(UNIT=unit,FILE=filename,STATUS="NEW",ACTION='WRITE',IOSTAT=ierror,IOMSG=err_string)
   ```

3. **打开一个临时文件 (给临时文件指定文件名是错误的)**

   ```Fortran
   OPEN(UNIT=12,STATUS='SCRATCH',IOSTAT=ierror)
   ```

### 4.5.2 CLOSE语句

 CLOSE语句关闭一个文件并释放与之关联的心单元号。其格式为：

```fortran
CLOSE(close_list)
```

因此，close_list中必须包含一个指定o/i单元号的子句。

如果在程序中没有包含对给定文件的CLOSE语句， 这个文件将在程序结束运行时被自动关闭。

### 4.5.3 磁盘文件的读和写

文件通过OPEN语句连接到i/o单元，就可以使用之前用过的READ和WRITE语句对文件进行读或写。

例如：

```fortran
OPEN(UNIT=8,FILE='INPUT.DAT',STATUS='OLD',IOSTAT=ierror)
READ(8,*)x,y,z
!以自由格式从文件INPUT.DAT中读取变量x,y,z的值。
OPEN(UNIT=9,FILE='OUTPUT.DAT',,STATUS='NEW',IOSTAT=ierror)
WRITE(9,100)x,y,z
100 FORMAT('x = ',F10.2,'y = ',F10.2,'z = ',F10.2)
!以特定的格式向文件OUTPUT.DAT中写入x,y,z的值。
```



### 4.5.4 READ语句中的IOSTAT和IOMSG子句

当使用磁盘文件时，可以给READ语句增加IOSTAT和IOMSG子句，用来检测输入文件。

1. IOSTAT

   格式：

   ```fortran
   IOSTAT=int_var
   !int_var是整型变量
   ```

   执行成功：返回int_var=0。

   执行失败：返回一个与系统错误信息对应的正数。

   ​					 如果由于已经到达输入数据文件的尾部而使语句执行失败， 就给该变量返回一个负数。

2. IOMSG

   格式：

   ```fortran
   IOMSG=chart_var
   !chart_var字符变量
   ```

   执行失败：返回以语句的形式来解释发生的错误。

   

   如果在READ语句中不存在IOSTAT，则**任何对文件结尾之外的行信息的读取操作都将使程序的运行异常中断**。这种行为在设计良好的程序中是不能接受的。

   我们经常要从文件中读取所有数据，直到到达文件的结尾，然后再对这些数据进行某些处理。这就是IOSTAT得以应用的地方：

   **如果存在IOSTAT＝子句， 程序不会由于读取文件尾之外的行信息而异常中断**。

   相反， **READ语句会完成执行， 并将IOSTAT变量设置为负数。**随后可以测试这个变量的值， 并对应地处理数据。
   
------
**示例：从文件中读取文件。要求：通过编写个程序来说明这个过程，程序可以从磁盘文件中读入未知个数的实数值，并检测磁盘文件中的数据尾。**

该程序的输入包括：
(1) 要打开的文件名。
(2) 包含在该文件中的数据。
程序的输出为数据文件中的输入值。在文件尾部输出一条提示信息， 告诉用户发现了多少个有效的输入数值。

源代码：

```fortran
PROGRAM read_file
IMPLICIT NONE 
REAL::data                      !文件数据
INTEGER::N=0                    !有效数值
integer::status                 !IOSTAT
CHARACTER(len=10)::file_name    !注意设置长度不然默认为1
CHARACTER(len=80)::msg          !IOMSG err_string

WRITE(*,1000)
1000 FORMAT('Please input filename:',/)
READ(*,*)file_name

WRITE(*,1010)file_name
1010 FORMAT("The input file name is: ",A)
!打开输入输出文件
!输入文件
OPEN(UNIT=100,FILE=file_name,STATUS='OLD',IOSTAT=status,IOMSG=msg)
!输出文件
OPEN(UNIT=110,FILE='OUTPUT.dat',STATUS='replace',IOSTAT=status,IOMSG=msg)
!进行数据操作

!打开文件成功
OPENIF:IF(status==0)THEN                                    

    readloop:DO                                             !读取数据
                READ(100,*,IOSTAT=status,IOMSG=msg)data     !读取N+1行数据
                IF(status/=0)EXIT                           !如果读取过程失败则退出循环
                N=N+1                                       !读取成功记录读取行数
                WRITE(110,'(F10.5)')data                    !计入到OUTPUT
                WRITE(*,'(F10.5)')data                      !显示到屏幕
             END DO readloop
             
!读取文件过程中失败
    readif:IF(status>0)THEN                             
                WRITE(*,1020)N+1                            !计数器需要加1行
                1020 FORMAT('An error occurred reading line：',I4)
                WRITE(*,*)msg
           ELSE
                WRITE(*,1030)N
                1030 FORMAT('End of file reached.',/,'There were',I4,&
                ' values in the file')
           END IF readif
           
!打开文件失败
    ELSE OPENIF                                             
        WRITE(*,1040)status
        1040 FORMAT('Error opening file:IOSTAT = ',I6)
        WRITE(*,1050)TRIM(msg)                !TRIM:舍去字符串尾部的空格，并返回剩余部分
        1050 FORMAT(A)
    END IF OPENIF

END PROGRAM read_file
```



### 4.5.5 文件定位

​	用处：需要在执行期间多次读取一块数据或者多次处理整个文件。

- **BACKSPACE**	

  每次调用都可以回退一条记录：

  ```fortran
  BACKSPACE(UNIT=unit)
  ```

- **REWIND**

  从文件头重新开始文件：

  ```fortran
  REWIND(UNIT=unit)
  ```

**两条语句还可以包含IOSTAT和IOMSG，检测回退或倒回操作期间是否发生错误，以不引起程序异常中断。**

------

示例：编写一个程序，接收一组非负的实数值， 并将其保存在一个临时文件中。数据输入后，程序应询问用户对什么数据记录感兴趣，然后从磁盘文件中重新找到并显示这个数值。(由于希望程序只读取正数或零值， 因此可以使用一个负数值作为终止对程序的输入操作的标记。)

关键：临时文件，文件定位

源代码(程序有拓展)：

```fortran
PROGRAM search_file
    IMPLICIT NONE
    !数据字典:声明常量
    INTEGER,PARAMETER::T_unit=8 !临时文件i/o号
    !数据字典::
    REAL::data                !输入数据
    INTEGER::icount=0         !输入数据个数
    REAL::irec                 !目标数据
    INTEGER::lines            !目标行数
    INTEGER::i                !循环
    INTEGER::sel              !选择功能
    INTEGER::judge=0        !判断是否搜索到
    !输入数据
    WRITE(*,*)'Please input the number'
    !打开临时文件
    !注意临时文件不用担心是否能创建失败 因此无需IOSTAT和IOSMSG(检测文件)
    !ACTION直接用默认值(replace),因为STATUS=SCRATCH,因此FILE也无需命名。
    !因此临时文件只需i/o号和status=scratch3
    !打开临时文件
    OPEN(UNIT=T_unit,STATUS='scratch')
    DO
        WRITE(*,90)icount+1
        90 FORMAT('Enter sample ',I5)
        READ(*,*)data
        WRITE(T_unit,*)data
        IF(data<0)EXIT
        icount=icount+1
    END DO

    WRITE(*,110)icount
    110 FORMAT('The number you input is:',I5)

!搜索数据行数或者输入行数搜索数据
    WRITE(*,10)
    10 FORMAT('(1)Enter data,search lines.    ',/,&
               '(2)Enter line,search data. ',/,&
               '(Enter 1 or 2)'&
               )
    READ(*,*)sel

!输入数据,输出行数
    IF(sel==1)THEN
        WRITE(*,120)
        120 FORMAT('Please input number what you want to search:')
        READ(*,*)irec
        REWIND(UNIT=T_unit)
        DO  i=1,icount
            READ(T_unit,*)data
            IF(data==irec)then
                WRITE(*,130)i
                130 FORMAT('Find the data in line :',I5)
                judge=1
                CYCLE
            ELSEIF(i==icount .AND. judge==0 )THEN
                WRITE(*,140)
                140 FORMAT('No data found!')
            END IF

        END DO
            WRITE(*,150)
            150 FORMAT('PROGRAM OVER!!')

!输入行数，输出数据
    ELSEIF(sel==2)then
        WRITE(*,160)icount
        160 FORMAT('Please enter the target line:','(','1','-',I3,')')
        READ(*,*)lines
        REWIND(UNIT=T_unit)
        !搜索目标行数据
        DO  i=1,icount
            READ(T_unit,*)data
            IF(i==lines)then
                WRITE(*,170)
                170 FORMAT('The data of the target line is:')
                WRITE(*,*)data
                EXIT
            ELSEIF(i==icount)THEN
                WRITE(*,180)
                180 FORMAT('Maximum number of lines exceeded!')
            END IF
        END DO
    ELSE
        WRITE(*,*)"INPUT ERROR!!!!"
    END IF
END PROGRAM


```
