# 9 字符变量的更多特性

## 9.1 字符比较操作

字符串可以相互比较，这是通过关系操作符，或者一些特定的字符比较函数（称为词汇函数，字符串函数）来实现。考虑到程序的移植性，字符串函数比关系操作具有更多优势。

比较规则：

1. 比较从每个字符串的第一个字符开始，如果相同，那么比较第二个字符。继续这种过程，直到发现字符串中第一个不同的字符。例如：'AAAAAB'>'AAAAAA'。
2. 如果字符串的长度不同，比较从每个字符串的第一个字母开始，这个过程持续到发现不同的字母。如果直到其中一个字符串的结尾，两个字符串都是相同的，那么就认为另一个字符串是比较大的。因此：'AB'>'AAAA', 'AAAAA'>'AAAA'

排序子例程的结果依赖千字符集，以及执行程序的处理器上使用的字符集。为了使程序移植性变强，利用Fortran内置逻辑函数可以精确地与关系操作符等效。

**字符串函数**：
$$
\bold{LLT}\cong\,<;\,\,\,
\bold{LLE}\cong\,\leqslant 
\\
\bold{LGT}\cong\,>;\,\,\,
\bold{LGE}\cong\,\geqslant
$$
它们总是按照ASCII排序的顺序来比较字符， 而不考虑程序在什么类型的计算机上运行。

## 9.2 内置字符函数

1. **CHAR、ACHAR**

   将输入的整型值转换为对应的输出字符

   ```fortran
   CHARACTER::out
   INTEGER::input =65
   OUT＝ CHAR(input)
   ```

   CHAR函数的输入是单个整型值，输出是字符，字符在特定处理器上的排序序号与函数的输入参数相等。

2. **ICHAR、IACHAR**

   将输入的字符转换为对应的整型数输出

   ```fortran
   CHARACTER::input='A'
   INTEGER::out
   out=ICHAR(input)
   ```

多用函数ACHAR和IACHAR，而不是函数CHAR和ICHAR，因为前者的运行结果**独立于它们所运行的处理器**，而后者的运行结果依赖于它们所运行的处理器所采用的排序序列。

3. **LEN**

   返回字符串声明的长度。输出是字符串声明的大小，而不是字符串中非空字符的个数。

   ```fortran
   CHARACER(len=20)::str1
   INTEGER::out
   strl ='ABC XYZ'
   out=LEN(str1)
   !out=20
   ```

4. **LEN_TRIM**

   返回字符串的长度，不记尾部的空字符。如果str1 是空字符串，那么函数LEN_TRIM返回0

   ```fortran
   CHARACTER(len=20)::str1
   INTEGER::out
   strl='ABC XYZ'
   out=LEN_TRIM(str1)
   !out=7
   ```

5. **TRIM**

   返回不含尾部空字符的字符串。

   ```fortran
   CHARACTER(len=20)::strl
   strl='ABC XYZ'
   WRITE(*,*)' '' ',TRIM(strl),' '' '
   ```

6. **INDEX**

   在字符串中查找某个模式串。该函数的输入是两个字符串：str1是被查找的字符串，str2是要查找的模式串。如果没有找到匹配的模式串， 那么INDEX 输出0。

   ```fortran
   CHARACTER(len=20)::str1='THIS IS A TEST!'
   CHARACTER(len=20)::str2='TEST'
   INTEGER::out
   out=INDEX(strl, str2)
   !out=11
   ```

部分通用地内置字符函数的使用：

| 函数名及参数            | 参数类型  | 返回值类型 | 说明                                                         |
| :---------------------- | --------- | ---------- | ------------------------------------------------------------ |
| ACHAR(ival)             | INT       | CHAR       | 返回ASCII排序序列中对应于值ival的字符                        |
| CHAR(ival)              | INT       | CHAR       | 返回处理器所用的排序序列中对应于值ival的字符                 |
| IACHAR(char)            | CHAR      | INT        | 返回ASCII排序序列中对应于char的整数值                        |
| ICHAR (char)            | CHAR      | INT        | 返回处理器所用排序序列中对应于char的整数值                   |
| INDEX (str1,str2, back) | CHAR，LOG | INT        | 返回str1中包含str2的第一个字符的序号(0=匹配)。参数back是可选的，如果提供且为真，那么从str1的末尾开始搜索， 而不是开始。 |
| LEN (str1)              | CHAR      | INT        | 返回str1的长度                                               |
| LEN_ TRIM (str1)        | CHAR      | INT        | 返回str1的长度， 不包括尾部的空格                            |
| LLT(str1,str2)          | CHAR      | LOG        | 根据ASCII排序序列， 如果str1<str2, 返回真                    |
| LLE(str1,str2)          | CHAR      | LOG        | 根据ASCII排序序列， 如果str1<=str2, 返回真                   |
| LGT(str1,str2)          | CHAR      | LOG        | 根据ASCII排序序列， 如果str1>str2, 返回真                    |
| LGE(strl,str2)          | CHAR      | LOG        | 根据ASCII排序序列， 如果str1>=str2, 返回真                   |
| TRIM (str1)             | CHAR      | CHAR       | 去除掉尾部的空格后， 返回str1                                |


## 9.3 把字符变量传入子例程或函数

```fortran
CHARACTER(len=*)::char_var
```

**char_var**：字符类形式参数的名称。

*：不确切的知道该字符变量的长度。

在过程中使用CHARACTER (len=＊)类型语句声明字符类形式参数。这一特性使得过程可以处理任意长度的宇符串。如果过程需要知道特定变量的确切长度， 可以用该变量作为参数调用LEN函数。

示例：用选择排序算法实现字符数组array的升序排序，子例程排序算法是按ASCII字符集排序数据子例程所需参数有字符数组元素值、数组元素个数，且处理过程与字符集无关。

```fortran
SUBROUTINE storc(array,n)
    IMPLICIT NONE
    INTEGER,INTENT(IN)::n !数据值的个数
    CHARACTER(len=*),DIMENSION(n),INTENT(INOUT)::array
    !用于排序的数组
    !声明局部变量
    INTEGER::i      !控制变量循环
    INTEGER::iptr   !指向最小值的指针
    INTEGER::j      !循环控制变量
    CHARACTER(len=len(array))::temp !临时变量
    !数组排序
    OUTER:DO i=1,n-1
            iptr=i
            inner:DO j=i+1,n
                    IF(LGE(array(iptr),array(j)))THEN
                        iptr=j

                    END IF
                    !寻找到最小值:array(itpr)
                  END DO inner
                IF(j/=i)THEN
                    !储存最小值至temp中交换ij数组
                    temp=array(iptr)
                    array(iptr)=array(i)
                    array(i)=temp
                END IF
          END DO OUTER
          WRITE(*,*)array
END SUBROUTINE

PROGRAM test
    IMPLICIT NONE
    INTEGER,PARAMETER::MAX=20
    !MAX每个元素的最大长度不满足用空格
    !MIN读取数组字符串的长度
    INTEGER,PARAMETER::MIN=12
    INTEGER::n=5
    CHARACTER(len=MAX),ALLOCATABLE,DIMENSION(:)::array1
    array1=[CHARACTER(len=MIN)::'AaaSSs','AAasS','ABC','ACSs','asda']

    CALL storc(array1,n)
END PROGRAM test
```



## 9.4 可变长字符函数

我们已经知道，子例程可以用CHARACTER (len=＊)声明语句来声明可变长度的字符串，并处理它们。

我们也可以创建一个自动长度字符函数，函数返回的长度有调用参数来指定。

```fortran
MODULE character_subs
	CONTAINS
	FUNCTION abc(n)
!目的：返回第一个含有字符集中字母N的字符串
		IMPLICIT NONE
		!声明调用参数
		INTEGER,INTENT(IN)::n
		CHARACTER(len=n)abc
			!声明局部变爵
			!返回字符串长度
			!返回的字符串
		character(len=26)::alphabet = 'abcdefghijklmnopqrstuvwxyz'
			!获得要返回的字符串
			abc=alphabet(1:n)
	END FUNCTION abc
END MODULE character
```

## 9.5 内部文件

如何将**数字数据转换为字符数据**， 或者将**字符数据转换为数字数据**，并没有介绍到。

在Fortran中为这种转换提供了一种特殊机制，称为**内部文件**。

在内部文件中，READ操作和WRITE操作发生在**内部字符缓冲区（内部文件）中**，而不是磁盘文件（外部文件）中。**任何能够写入外部文件的数据都可以写入内部文件**，而且也可以执行更多的操作。同样，**任何可以从外部文件中读到的内容， 也可以从内部文件中读到。**

从一个内部文件中执行READ操作的通用格式如下：

```fortran
READ(buffer,format)argl,arg2,...
```

这里buffer 是指输入**字符缓冲区**，format是作用于READ的格式，arg1,arg2等变量的值将从缓冲区中读取。从内部文件中执行WRITE操作的通用格式为：

```fortran
WRITE(buffer,format)arg1,arg2,...
```

这里buffer是指输出字符缓冲区，format是作用千WRITE的格式，arg1, arg2等是将要被写入缓冲区的值。
内部文件通常用于将**字符数据转换为数字数据**，反之亦然。例如，如果字符变量input包含字符串'135.4'，那么下面的代码将把字符数据转换为实数：

```fortran
CHARACTER(len=5)::input='l35.4'
REAL::value
READ(input,*)value
```

**某些I/O特性对内部文件无效**，例如**OPEN**, **CLOSE**, **BACKSPACE**以及**REWIND**语句**不允许用于内部文件上**。

使用内部文件将宇符格式的数据转换为数字格式的数据， 反之亦然。

## 9.6 小结

### 9.6.1 遵循原则

1. 如果程序有可能必须在具有不同字符集的计算机上运行，在判断两个字符串是否相等的时候，得使用逻辑函数LLT, LLE, LGT和LGE。不要对字符串使用关系操作符<, <=，＞，＞＝， 因为它们的结果可能会根据计算机的不同而不同。

2. 使用ACHAR函数和IACHAR函数，而不是CHAR函数和ICHAR函数。因为前两个函数的运行结果独立千它们所运行的处理器，而后两个函数的运行结果很大程度上依赖它们所运行的处理器所采用的字符排序序列。
3. 在过程中使用**CHARACTER(len=＊)**类型语句来声明字符类**形式参数**。这个特性使得过程**可以操作任意长度的字符串**。如果子例程或者函数需要知道特定变量的实际长度，可以用该变量做参数调用**LEN函数**。
4. 使用内部文件把数据从字符格式转换为数字格式，反之也一样

### 9.6.2 语法小结

1. 内部READ语句

   ```fortran
   !格式
   READ(buffer,fmt)input_list
   !例子
   READ(line,'(1x,I10,F10.2)')i,slope
   ```

   内部READ语句根据fmt指定的格式把数据读入到输入列表中，可以是字符串、字符变量、FORMAT语句标号，或者＊。数据从内部字符变量buffer中读取。

2. 内部WRITE语句

   ```fortran
   !格式
   WRITE(buffer,fmt)output list
   !例子
   WRITE (line,'(2I10, F10.2)') i,j,slope
   ```

   内部WRITE语句根据fmt中指定的格式将数据写到输出列表中去，可以是字符串、字符变量、FORMAT语句的标号，或者＊。数据被写入内部字符变量buffer中。



