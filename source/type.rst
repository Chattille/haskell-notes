.. highlight:: haskell
   :linenothreshold: 10

====
类型
====

类型声明
========

.. code-block::

   <name>, ... :: <type>

- ``::``\ ：声明变量类型；
- 同一类型的数据必定具有相同操作；
- 可同时声明多个变量，不同变量用逗号分隔；
- Haskell 为强静态类型语言，一切皆有类型，因此必须声明变量的类型，提高代码的安全性；
- Haskell 支持类型推断，因此无需声明所有变量；
- GHCi 中，\ ``:type``\ 或\ ``:t``\ 可查看表达式类型；

  .. code-block::

     Prelude> :type 'a'
     'a' :: Char
     Prelude> :type True
     True :: Bool
     Prelude> :t 4 == 5
     4 == 5 :: Bool
     Prelude> :t "Hello!"
     "Hello!" :: [Char]

变量声明
========

.. code-block::

   <name> = <value>

- ``=``\ ：

  - 声明变量的值为\ ``value``\ ；
  - Haskell 中，\ ``=``\ 表示\ :tr:`定义 (definition)`\ ，而非\ :tr:`赋值 (assignment)`\ ；

- 命名：

  - 大小写拉丁字母、数字、下划线以及\ **单引号**\ ``'``\ 为合法字符，但只能以\ **小写字母或下划线**\ 开头；
  - Haskell 通常使用驼峰命名法；

- 变量一旦声明，在同一程序内便\ **不能更改**\ ，否则报错；
- 在 Haskell 中，变量不是储存数据的容器，\ **变量只是数据的别名**\ ；

.. code-block::
   :caption: test.hs
   :linenos:

   x :: Int  -- x 有类型 Int
   x = 3     -- x **默认** 为 3
   x = 4
   -- test.hs:3:1: error:
   --     Multiple declarations of ‘x’
   --      Declared at: test.hs:2:1
   --                   test.hs:3:1

基本数据类型
============

- ``Int``\ ：整数，最值取决于 CPU 架构，在 64 位系统上，\ ``Int``\ 的取值范围为\ :math:`\pm2^{63}`\ ；

  .. code-block::

     biggestInt, smallestInt :: Int
     biggestInt = maxBound  -- 该机器上的最大整数
     smallestInt = minBound -- 该机器上的最小整数

- ``Integer``\ ：整数，最值取决于计算机内存容量；

  .. code-block::

     reallyBig :: Integer
     reallyBig = 2 ^ (2 ^ (2 ^ (2 ^ 2)))

- ``Float``\ ：单精度浮点数，用 32 位来表示；

  .. code-block::

     f1, f2 :: Float
     f1 = 4.584234
     f2 = 6.2831e-30

- ``Double``\ ：双精度浮点数，用 64 位来表示；

  .. code-block::

     d1, d2 :: Double
     d1 = 4.584234234230943
     d2 = 6.2831e-300

- ``Bool``\ ：布尔值，只有\ ``True``\ 和\ ``False``\ 两种；

  .. code-block::

     b1, b2 :: Bool
     b1 = True
     b2 = False

- ``Char``\ ：单个 Unicode 字符，由单引号包围；

  .. code-block::

     c1, c2 :: Char
     c1 = 'x'
     c2 = '棒'

- ``String``\ ：字符串，由双引号包围，其实质是一组字符列表；

  .. code-block::

     s :: String
     s = "Hello, Haskell!"

类型注释
========

.. code-block::

   <object> :: <type>

- :tr:`类型注释 (type annotation)`\ ：对对象类型的注释，指定对象的类型；
- 类型注释不一定出现在一行的末尾，可以通过括号改变类型注释的优先级；

.. code-block::

   exp1 = read "2" -- *** Exception: Prelude.read: no parse
   exp2 = read "2" :: Int        -- 2
   exp3 = read "2" :: Float      -- 2.0
   exp4 = succ (read "2" :: Int) -- 3

类型变量
========

- :tr:`类型变量 (type variable)`\ ：表示任意类型，即该表达式类型无关；

  .. note::

     类型变量与其他语言中的\ :tr:`泛型函数 (generic function)`\ 类似，但更强大。

- :tr:`多态函数 (polymorphic function)`\ ：使用了类型变量的函数；
- :tr:`多态常量 (polymorphic constant)`\ ：使用了类型变量且仅声明了一个类型的变量；
- 命名：

  - Haskell 允许类型变量为多字符，但约定俗成以单个小写字母表示；

    .. code-block::

       head :: [a] -> a
       -- 函数 'head' 取出并返回列表的第一个元素，列表元素可为任意类型

  - 不同字符不一定代表不同类型；

    .. code-block::

       fst :: (a, b) -> a
       -- 'a' 和 'b' 为不同的类型变量，但两者的类型不一定不同
       -- 这里仅表示返回值的类型与数对中的第一个元素类型相同

类型类
======

.. code-block::

   (<typeClass> <typeVariable>, ...) => <typeVariable> -> ...

- :tr:`类型类 (type class)`\ ：

  - 用于约束类型的接口；
  - 一种类型可以是多个类型类的成员，一个类型类也可以有多个类型成员；

- 语法格式：

  - 类型约束：

    - 使用小括号\ ``()``\ 包围，在此约束类型；
    - ``<typeClass> <typeVariable>``\ 表示类型变量\ ``<typeVariable>``\ 为类型类\ ``<typeClass>``\ 的成员；
    - 多个类型约束用逗号\ ``,``\ 分隔；
    - 当只有一个类型约束时，括号可省略；

  - ``=>``\ ：定义类型约束，分隔类型约束和类型变量；
  - 类型变量：语法与\ :ref:`函数定义 <function:函数定义>`\ 相同；

  .. code-block::

     (==) :: Eq a => a -> a -> Bool
     -- '(==)' 接受两个任意类型的参数并返回布尔值
     -- 类型变量 'a' 属于 'Eq' 类型类

常用类型类
----------

.. _type-class-eq:

- ``Eq``\ ：可判断相等性（\ ``==``\ 和\ ``/=``\ ），凡可比较相等性的类型都属于此类；

  .. code-block::

     elem :: (Foldable t, Eq a) => a -> t a -> Bool
     -- 若参数存在于列表中，则函数 'elem' 返回 'True'（成员判断）
     -- 该函数使用 '(==)' 和 '(/=)' 进行判断

- ``Ord``\ ：可比较大小；

  - 除函数外，前文中的所有类型都属于此类；
  - ``Ord``\ 类型类的成员也必须是\ ``Eq``\ 的成员；

  .. code-block::

     compare :: Ord a => a -> a -> Ordering
     -- 函数 'compare' 比较两个参数，并返回表示两者关系的字符串
     -- 'GT'（大于），'EQ'（等于），或 'LT'（小于）
     -- 比较的参数属于 'Ord' 类型类

- ``Show``\ ：可用字符串表示，除函数外的所有类型都属于此类；

  .. code-block::

     show :: Show a => a -> String

- ``Read``\ ：可将字符串转为\ ``Read``\ 的某个成员；

  .. code-block::

     read :: Read a => String -> a

- ``Enum``\ ：可枚举，有\ ``()``\ 、\ ``Bool``\ 、\ ``Char``\ 、\ ``Ordering``\ 、\ ``Int``\ 、\ ``Integer``\ 、\ ``Float``\ 和\ ``Double``\ ，该类型的值具有后继值和前趋值；

  .. code-block::

     enumFromTo :: Enum a => a -> a -> [a]

- ``Bounded``\ ：具有上限和下限；

  .. code-block::

     minBound :: Bounded a => a -- 多态常量

- ``Num``\ ：具有数字属性，所有数字都是多态常量；

  .. code-block::

     (*) :: Num a => a -> a -> a

- ``Integral``\ ：整数数字，包含成员\ ``Int``\ 和\ ``Integer``\ ；

  .. code-block::

     fromIntegral :: (Integral a, Num b) => a -> b

- ``Floating``\ ：浮点数数字，包含成员\ ``Float``\ 和\ ``Double``\ ；

  .. code-block::

     cos :: Floating a => a -> a

- ``Foldable``\ ：可折叠类型，可理解为可迭代序列，\ ``Foldable t => t a``\ 表示数据为可折叠类型\ ``t``\ ，该数据下的元素类型为\ ``a``\ ；

  .. code-block::

     foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
