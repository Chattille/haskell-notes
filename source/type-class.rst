.. highlight:: haskell
   :linenothreshold: 10

======
类型类
======

声明
====

.. code-block::

   class <typeClass> <typeVariable> where
       <functionDeclaration>
       ...

- :tr:`类型类 (type class)`\ ：约束类型的接口，同一类型类下的类型具有相同行为、能使用该类型类下所定义的方法；

  .. code-block::

     class Eq a where
         (==) :: a -> a -> Bool -- 任何属于 'Eq' 类型类的成员
         (/=) :: a -> a -> Bool -- 都可使用这两个函数

- 语法格式：

  - ``class``\ 关键字定义一个类型类；
  - 类型类名称需大写字母开头；
  - 类型变量表示该类型类的成员，使用小写字母；
  - ``where``\ 关键字定义类型类体；
  - 类型类体即一系列函数声明，不需要定义函数体；

- ``deriving``\ :ref:`关键字 <algebraic-data-type:派生实例>`\ 可自动将数据类型转换为类型类的实例，使得该数据类型能使用类型类下定义的方法；

.. code-block::

   -- | 命令式语言类似的布尔值：
   -- 非空为 'False'，否则为 'True'。
   -- 'YesNo' 类型类的实例成员都能使用该类型类下定义的函数。
   class YesNo a where
       yesno :: a -> Bool

   -- | '0' 为 'False'，否则为 'True'。
   instance YesNo Int where
       yesno 0 = False
       yesno _ = True

   -- | 空列表为 'False'，否则为 'True'。
   instance YesNo [a] where
       yesno [] = False
       yesno _  = True

   -- | 'id' 函数按原样返回接受到的值。
   instance YesNo Bool where
       yesno = id

   exp1 = yesno (0 :: Int) -- False
   exp2 = yesno ""         -- False
   exp3 = yesno []         -- False
   exp4 = yesno "alice"    -- True

实例
====

.. code-block::

   instance <typeClass> <type> where
       <functionBodies>
       ...

- 类型类的实例享用类型类定义的方法；
- 语法格式：

  - ``instance``\ 关键字定义类型类实例；
  - 类型类为被派生的类型类；
  - 类型指定该类型为类型类实例/成员；
  - ``where``\ 关键字定义实例体；
  - 实例体即一系列函数体，指定该类型该如何执行函数；

- 部分应用：类型类的实质也是函数，经过柯里化，因此可以接受少于形参数量的实参，实现部分应用；
- 类型类实例支持模式匹配；

.. code-block::

   data TrafficLight = Red | Yellow | Green

   instance Show TrafficLight where
       show Red    = "Red light"
       show Yellow = "Yellow light"
       show Green  = "Green light"

   instance Eq TrafficLight where
       Red    == Red    = True
       Yellow == Yellow = True
       Green  == Green  = True
       _      == _      = False

   exp1 = Red          -- Red light
   exp2 = Green        -- Yellow light
   exp3 = Red == Green -- False
   exp4 = Red `elem` [Red, Yellow, Green] -- True

类型约束
========

.. code-block::

   instance (<typeClass> <typeVariable>, ...) => <type> where

- 类型类支持\ :ref:`类型约束 <type:类型类>`\ 语法，意义相似；

.. code-block::

   infixr 5 :^:
   data Tree a = Leaf a | Tree a :^: Tree a
       deriving (Show, Read)

   -- | 'a' 是 'Eq' 的成员，同时也是 'Tree' 的成员。
   -- 同时，'Tree' 本身也属于 'Eq'。
   instance Eq a => Eq (Tree a) where
       Leaf a  == Leaf b  = a == b
       m :^: n == x :^: y = m == x && n == y
       _       == _       = False

   -- | 同上。
   instance Ord a => Ord (Tree a) where
       Leaf a  <= Leaf b  = a <= b
       Leaf a  <= m :^: n = True
       m :^: n <= Leaf a  = False
       m :^: n <= x :^: y = m < x || m == x && n <= y

   exp1 = Leaf 1 :^: Leaf 2 :^: Leaf 3
          -- Leaf 1 :^: (Leaf 2 :^: Leaf 3)
   exp2 = Leaf 1 :^: Leaf 2 <= Leaf 1 :^: Leaf 3 -- True
   exp3 = Leaf 5 <= Leaf 2 :^: Leaf 3            -- True

种类
====

.. code-block::

   <type> :: <kind>

- :tr:`种类 (kind)`\ ：类型构造器的类型；
- GHCi 中使用\ ``:kind``\ 命令可查看类型构造器的种类；
- 语法格式：

  - ``*``\ ：该类型为\ :tr:`具体类型 (nullary type/monotype)`\ ，不接受类型参数；

    .. code-block::

       Int :: *       -- 'Int' 为具体类型
       Maybe Int :: * -- 'Maybe Int' 同上

  - ``P -> Q``\ ：接受一个类型\ ``P``\ ，返回一个类型\ ``Q``\ ；

    .. code-block::

       Maybe :: * -> * -- 接受一个具体类型（Int）
                       -- 并返回具体类型（Maybe Int）
       Either :: * -> * -> *   -- 接受两个
       Either String :: * -> * -- 部分应用
       (->) :: * -> * -> *
