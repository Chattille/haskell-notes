.. highlight:: haskell
   :linenothreshold: 10

============
代数数据类型
============

简介
====

定义
----

.. code-block::

   data <name> = <valueConstructor> <type> ... | ...
       deriving (<typeClass>, ...)

- :tr:`代数数据类型 (algebraic data type)`\ ：定义了各元素形态的数据类型，“代数”指“和”和“积”；

  - :tr:`和 (sum)`\ ：分支，即 A 或 B；
  - :tr:`积 (product)`\ ：组合，即 A 和 B；

- 语法格式：

  - \ ``data``\ 关键字：定义一个新数据类型；

    - 命名：与变量相同，但只能以\ **大写字母**\ 开头；

  - 数据类型名称：\ ``data``\ 关键字后跟数据类型名称；
  - :tr:`值构造器 (value constructor)`\ ：\ ``=``\ 之后跟值构造器，定义该类型下的不同值，首字母大写，可用于模式匹配；

    - 值构造器的\ **本质为函数**\ ，因此之后所跟的类型其实是该值构造器的参数；
    - 当数据类型只包含一种值构造器时，值构造器与类型名称可以同名；

    .. code-block::

       data Bool = False | True
       -- 一个 'Bool' 类型可以为 'False' 或 'True' 两个值之一

  - 类型：值构造器之后可添加任意多个类型，表示该值构造器下会包含该种类型；
  - ``|``\ ：表示或，分隔多个形态；

  .. _deriving:

  - ``deriving``\ 关键字：代数数据类型声明之后可跟\ ``deriving``\ 关键字，表示该数据类型为指定类型类的成员；
  - 类型类：\ ``deriving``\ 关键字后跟一或多个类型类，表示该数据类型为该类型类的成员，只有一个类型类时括号可省略；

.. code-block::

   data Shape = Circle Float Float Float          -- ^ 圆形
              | Rectangle Float Float Float Float -- ^ 矩形
              deriving Show -- 可使用函数 'show'

   -- | 计算形状的面积。
   surface :: Shape -> Float
   surface (Circle _ _ r) = pi * r ^ 2
   surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

   exp1 = surface $ Circle 10 20 10       -- 314.15927
   exp2 = surface $ Rectangle 0 0 100 100 -- 10000.0
   exp3 = map (Circle 10 20) [4, 5]
          -- [Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0]

模式匹配
--------

- 代数数据类型可进行模式匹配；

  .. code-block::

     foo (Constr1 a b)   = ...
     foo (Constr2 a)     = ...
     foo (Constr3 a b c) = ...
     foo Constr4         = ...

  - 当构造器后跟类型时必须用括号括起来，因为构造器的本质为函数；

- ``_``\ ：可作为通配符使用，表示匹配任何值；
- ``var@pat``\ ：对\ ``pat``\ 进行模式匹配，并用\ ``var``\ 捕捉整个模式；

  .. code-block::

     data Person = Person String String Int
         deriving Show

     getName :: Person -> String
     getName person@(Person name _ _) =
       "The name field of " ++ show person ++ " is " ++ name

     exp4 = getName $ Person "Alice" "Liddell" 28
     -- "The name field of Person \"Alice\" \"Liddell\" 28 is Alice"

记录语法
========

.. code-block::

   data <name> = <valueConstructor> {<field> :: <type>, ...} | ...
       deriving (<typeClass>, ...)

- :tr:`记录语法 (record syntax)`\ ：自动为对应字段创建相应函数；
- 记录语法可为对应字段创建函数，这样调用函数时可返回对应字段的值，且使用时参数无需按顺序传入；
- 语法格式：

  - ``data``\ 关键字同上；
  - 值构造器后跟大括号\ ``{}``\ ，括号内为各字段；
  - 字段名后跟类型；
  - ``deriving``\ 关键字同上；

.. code-block::

   data Car = Car
       { company :: String -- ^ company :: Car -> String
       , model   :: String -- ^ model :: Car -> String
       , year    :: Int    -- ^ year :: Car -> Int
       }
       deriving Show

   ford = Car { company = "Ford", year = 1967, model = "Mustang" }
   -- ford = Car "Ford" "Mustang" 1967 (同样合法)

   exp1 = company ford -- "Ford"
   exp2 = model ford   -- "Mustang"
   exp3 = year ford    -- 1967

类型构造器
==========

.. code-block::

   data <typeConstructor> <typeParameter> = ...

- :tr:`类型构造器 (type constructor)`\ ：本身不是一种类型，但能接受类型作为参数，生成新的类型；
- :tr:`类型参数 (type parameter)`\ ：类型构造器的参数，表示接受任意类型；

  .. code-block::

     data [] a = [] | a : [a]
     -- 'a' 表示该类型构造器接受任意类型，如 [Char]，[Int]，或 [String]

     data Maybe a = Nothing | Just a
     -- 'a' 代表任意类型
     -- Just 'a' :: Maybe Char

派生实例
========

- 类型类定义了一组行为，属于该类型类的类型均能执行该组行为；

  .. note::

     ``Eq``\ 类型类的成员可对其值应用\ ``==``\ 和\ ``/=``\ 函数，\ ``Ord``\ 类型类的成员可对其值应用\ ``>``\ 、\ ``<``\ 、\ ``>=``\ 、\ ``<=``\ 、\ :hs:func:`max`\ 、\ :hs:func:`min`\ 和\ :hs:func:`compare`\ 函数。

- ``deriving``\ 关键字：可将指定类型类的行为派生到代数数据类型中的所有值构造器上，使当前数据类型成为类型类的实例；

  - 值构造器中的所有字段也必须为该类型类的成员，\ ``deriving``\ 关键字才有效；

.. code-block::

   data Person = Person { firstName :: String
                        , lastName  :: String
                        , age       :: Int
                        }
                        deriving (Eq, Ord)

   data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
       deriving (Ord, Eq, Enum, Bounded, Show, Read)

   alice = Person "Alice" "Liddell" 28
   marie = Person "Marie" "Curie" 67
   exp1 = alice == marie -- False
          -- 首先比较值构造器
          -- 若相等，则比较值构造器中的值
   exp2 = alice == alice -- True
   exp3 = alice `elem` [alice, marie] -- True
   exp4 = Mon == Tue      -- False
   exp5 = succ Fri        -- Sat
   exp6 = [Mon .. Fri]    -- [Mon,Tue,Wed,Thu,Fri]
   exp7 = minBound :: Day -- Mon

类型同义词
==========

.. code-block::

   type <name> <typeParameter> ... = <type>

- :tr:`类型同义词 (type synonym)`\ ：为类型名指定一个同义词，该词与原类型名等效，可提高类型签名的可读性；
- ``type``\ 关键字定义类型同义词；

  .. code-block::

     type String = [Char]

- 类型参数：类型同义词也接受类型参数；

  .. code-block::

     type AssocList k v = [(k, v)] -- AssocList Int String
     type IntMap v = Map Int v     -- 部分应用

.. code-block::

   import qualified Data.Map as M

   data LockerState = Taken | Free deriving (Show, Eq)
   type Code = String
   type LockerMap = M.Map Int (LockerState, Code)

   -- | 查询锁柜编码，并检查占用情况。
   lockerLookup :: Int -> LockerMap -> Either String Code
   lockerLookup number lmap = case M.lookup number lmap of
       Nothing -> Left $ "No locker number " ++ show number ++ "!"
       Just (state, code) -> if state /= Taken
           then Right code
           else Left $ "Locker " ++ show lockerNumber ++ " is taken!"

   lockers :: LockerMap
   lockers = M.fromList
       [ (100, (Taken, "2D39I"))
       , (101, (Free, "JAH3I"))
       , (103, (Free, "IQSA9"))
       , (105, (Free, "QOTSA"))
       , (109, (Taken, "893JJ"))
       ]

   exp1 = lockerLookup 100 lockers -- Left "Locker 100 is taken!"
   exp2 = lockerLookup 101 lockers -- Right "JAH3I"
   exp3 = lockerLookup 102 lockers -- Left "No locker number 102!"
   exp4 = lockerLookup 105 lockers -- Right "QOTSA"

递归数据结构
============

- 代数数据类型可递归定义；

  .. code-block::

     data List a = Empty
                 | Cons a (List a)
                 deriving (Show, Read, Eq, Ord)
          -- 列表可为空列表，
          -- 或用构造器联接一个元素与另一列表（另一列表也可为空列表）

     exp1 = 3 `Cons` Empty            -- Cons 3 Empty
     exp2 = 4 `Cons` (3 `Cons` Empty) -- Cons 4 (Cons 3 Empty)

- 固定性声明同样可用于代数数据类型定义中；

  .. code-block::

     infixr 5 :-: -- 自定义运算符
     data List a = Empty
                 | a :-: List a
                 deriving (Show, Read, Eq, Ord)

     exp3 = 3 :-: Empty             -- 3 :-: Empty
     exp4 = 2 :-: exp3              -- 2 :-: (3 :-: Empty)
     exp5 = 1 + 1 :-: (3 :-: Empty) -- 2 :-: (3 :-: Empty)

- 通过递归数据结构可以定义更复杂的行为；

  .. code-block::

     -- | 
     -- Module      : Tree
     -- Description : 树相关模块。
     module Tree where

     -- | 'Tree' 可以是 'EmptyNode'，或由一个值和另外两个 'Tree' 构成的节点。
     data Tree a = EmptyNode
                 | Node a (Tree a) (Tree a)
                 deriving (Show, Read, Ord, Eq)

     -- | 生成只有一个节点的 'Tree'。
     singleton :: a -> Tree a
     singleton x = Node x EmptyNode EmptyNode

     -- | 将值插入 'Tree'。
     -- 若该值大于当前节点的值，则插入右侧的 'Tree'；
     -- 否则，插入左侧的 'Tree'。
     --
     -- ==== __例子:__
     -- >>> treeInsert 3 (singleton 5)
     -- Node 5 (Node 3 EmptyNode EmptyNode) EmptyNode
     --
     -- >>> treeInsert 7 (singleton 5)
     -- Node 5 EmptyNode (Node 7 EmptyNode EmptyNode)
     treeInsert :: Ord a => a -> Tree a -> Tree a
     treeInsert x EmptyNode = singleton x
     treeInsert x (Node base left right)
         | x == base = Node base left right
         | x < base  = Node base (treeInsert x left) right
         | x > base  = Node base left (treeInsert x right)

     -- | 该值是否在 'Tree' 中？
     --
     -- ==== __例子:__
     -- >>> 3 `treeElem` (singleton 3)
     -- True
     --
     -- >>> 3 `treeElem` (singleton 5)
     -- False
     treeElem :: Ord a => a -> Tree a -> Bool
     treeElem x EmptyNode = False
     treeElem x (Node base left right) | x == base = True
                                       | x < base  = treeElem x left
                                       | x > base  = treeElem x right

新类型
======

.. code-block::

   newtype <TypeName> <typeParameter> = <Constructor> <field>

- ``newtype``\ 关键字将已有类型封装到新类型中，语法同\ ``data``\ :ref:`关键字 <algebraic-data-type:定义>`\ 基本类似；
- ``newtype``\ 关键字只能新建只有一个值构造器的类型，且该值构造器只能有一个字段；
- 比起\ ``data``\ :ref:`关键字 <algebraic-data-type:定义>`\ ，\ ``newtype``\ 关键字不会对类型进行经常性封装，因此速度更快；
- ``newtype``\ 关键字也可以配合\ ``deriving``\ :ref:`关键字 <deriving>`\ 使用
- ``newtype``\ 关键字定义的类型不属于原类型所属的任何类型类，因此需要手动\ :ref:`派生实例 <algebraic-data-type:派生实例>`\ ；

  .. code-block::

     -- | 将元组封装入新类型 'Pair'，即 @(a, b) -> Pair b a@。
     -- 用 @data@ 关键字定义的效果相同，但速度不及 @newtype@ 关键字。
     --
     -- ==== __例子：__
     --
     -- >>> fmap (+1) (1, 3)
     -- (1,4)
     --
     -- >>> getPair $ fmap (+1) (Pair (1, 3))
     -- (2,3)
     newtype Pair b a = Pair { getPair :: (a, b) }

     instance Functor (pair c) where
         fmap f (Pair (x, y)) = Pair (f x, y)

- 惰性：Haskell 本身是惰性的，而\ ``newtype``\ 关键字的惰性更强；

  - ``undefined``\ 异常代表计算错误，直接对其进行求值会抛出异常；

    .. code-block::

       Prelude> undefined
       *** Exception: Prelude.undefined
       CallStack (from HasCallStack):
         error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
         undefined, called at <interactive>:1:1 in interactive:Ghci1

  - 假设有以下类型\ ``CoolBool``\ ，若传入\ ``undefined``\ ，则 Haskell 会先进行模式匹配，但由于\ ``undefined``\ 不能模式匹配，因此会抛出异常；

    .. code-block::

       data CoolBool = CoolBool { getCoolBool :: Bool }

       -- >>> helloMe undefined
       -- "*** Exception: Prelude.undefined
       -- CallStack (from HasCallStack):
       --   error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
       --   undefined, called at <interactive>:2:9 in interactive:Ghci2
       helloMe :: CoolBool -> String
       helloMe (CoolBool _) = "hello"

  - 但如果使用\ ``newtype``\ 关键字，由于\ ``newtype``\ 定义的类型只能有一个值构造器和一个字段，因此 Haskell 不会进行模式匹配，而是直接进行求值；

    .. code-block::

       newtype CoolBool = CoolBool { getCoolBool :: Bool }

       -- >>> helloMe undefined
       -- "hello"
       helloMe :: CoolBool -> String
       helloMe (CoolBool _) = "hello"
