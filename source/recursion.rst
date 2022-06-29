.. highlight:: haskell
   :linenothreshold: 10

====
递归
====

简介
====

- :tr:`递归 (recursion)`\ ：在表达式内部定义表达式自己；

  .. note::

     斐波那契的数学定义即为一种递归：

     .. math::

        \begin{equation}
        F(n) = \left \{
        \begin{aligned}
        &0 &n = 0 \\
        &1 &n = 1 \\
        &F(n-1) + F(n-2) &n \ge 2
        \end{aligned}
        \right.
        \end{equation}

- 在 Haskell 中，递归十分重要，因为 Haskell 通过声明表达式来计算，所以 Haskell 中没有命令式语言中的\ ``for``\ 或\ ``while``\ 循环结构，而递归可以用于声明表达式，实现遍历等操作；

  .. code-block::

     quicksort :: Ord a => [a] -> [a]
     quicksort [] = []
     quicksort (x : xs) =
         let smallerSorted = quicksort [ a | a <- xs, a <= x ]
             biggerSorted  = quicksort [ b | b <- xs, b > x ]
         in  smallerSorted ++ [x] ++ biggerSorted

递归模式
========

``map``\ 模式
-------------

- ``map``\ 模式可实现列表的遍历，对列表中的每个元素进行相同的操作；
- 思路：

  - 对列表每个元素进行加一：

    .. code-block::

       addOne :: Num a => [a] -> [a]
       addOne [] = []
       addOne (x : xs) = x + 1 : addOne xs

       exp1 = addOne [1, 2, 3] -- [2,3,4]

  - 对列表每个元素进行平方：

    .. code-block::

       squareAll :: Num a => [a] -> [a]
       squareAll [] = []
       squareAll (x : xs) = x ^ 2 : squareAll xs

       exp2 = squareAll [1, 2, 3] -- [1,4,9]

  - 两函数在结构上重复，为遵循 DRY 原则，可以定义函数\ ``mapList``\ ，对两个函数进行抽象：

    .. code-block::

       mapList :: Num a => (a -> a) -> [a] -> [a]
       mapList f [] = []
       mapList f (x : xs) = f x : mapList f xs

       exp3 = mapList (+ 1) [1, 2, 3] -- [2,3,4]
       exp4 = mapList (^ 2) [1, 2, 3] -- [1,4,9]

  - :hs:func:`~GHC.Base.map`\ 函数即为该模式的实现；

``filter``\ 模式
----------------

- ``filter``\ 模式可实现列表的筛选；
- 思路：

  - 保留列表中大于三的元素：

    .. code-block::

       gt3 :: (Num a, Ord a) => [a] -> [a]
       gt3 [] = []
       gt3 (x : xs) | x > 3     = x : gt3 xs
                    | otherwise = gt3 xs

       exp1 = gt3 [1, 5, 2, 7, 3, 6] -- [5,7,6]

  - 保留列表中平方值小于十的元素：

    .. code-block::

       sqLt10 :: (Num a, Ord a) => [a] -> [a]
       sqLt10 [] = []
       sqLt10 (x : xs) | x ^ 2 < 10 = x : sqLt10 xs
                       | otherwise  = sqLt10 xs

       exp2 = sqLt10 [1, 5, 2, 7, 3, 6] -- [1,2,3]

  - 两函数在结构上重复，为遵循 DRY 原则，可定义函数\ ``filterList``\ ，对两个函数进行抽象：

    .. code-block::

       filterList :: (Num a, Ord a) => (a -> Bool) -> [a] -> [a]
       filterList f [] = []
       filterList f (x : xs) | f x       = x : filterList f xs
                             | otherwise = filterList f xs

       exp3 = filterList (> 3) [1, 5, 2, 7, 3, 6]              -- [5,7,6]
       exp4 = filterList (\x -> x ^ 2 < 10) [1, 5, 2, 7, 3, 6] -- [1,2,3]

  - :hs:func:`~GHC.List.filter`\ 函数即为该模式的实现；

``fold``\ 模式
--------------

- ``fold``\ 模式可根据前一个元素计算后的状态对当前元素进行相应计算；
- 思路：

  - 求列表所有元素和起始值的和：

    .. code-block::

       addAll :: Num a => a -> [a] -> a
       addAll a [] = a
       addAll a (x : xs) = addAll (a + x) xs

       exp1 = addAll 5 [1, 2, 3] -- 11

  - 求列表所有元素和起始值的积：

    .. code-block::

       multiAll :: Num a => a -> [a] -> a
       multiAll a [] = a
       multiAll a (x : xs) = addAll (a * x) xs

       exp2 = multiAll 2 [1, 2, 3] -- 12

  - 两函数在结构上重复，为遵循 DRY 原则，可定义函数\ ``foldList``\ ，对两个函数进行抽象：

    .. code-block::

       foldList :: (Num a, Num b) => (b -> a -> b) -> b -> [a] -> b
       foldList f z [] = z
       foldList f z (x : xs) = foldList f (f z x) xs

       exp3 = foldList (+) 5 [1, 2, 3] -- 11
       exp4 = foldList (*) 2 [1, 2, 3] -- 12

  - ``fold*``\ 函数即为该模式的实现；

``fold*``\ 函数
===============

- ``fold*``\ 函数：用起始值和运算函数，将序列反复“折叠”，直至最后剩下唯一一个值；
- 流程：

  1. 接受一个二目运算函数、一个起始值和一个序列作为参数；
  2. 从起始值和序列的第一个值或最后一个值开始，用运算函数进行计算，得到新的起始值；

     .. code-block::

        res = foldl f s [a, b, c, d]
        -- 等价于 ((((s `f` a) `f` b) `f` c) `f` d)

  3. 重复上一步，直到将序列“折叠”至一个值；
  4. 返回该值；

  .. code-block::

     res = foldl (+) 2 [1, 2, 3, 4]
     -- 2 + 1
     --    [1,2,3,4]
     -- 3 + 2
     --    [2,3,4]
     -- 5 + 3
     --    [3,4]
     -- 8 + 4
     --    [4]
     -- 12

- ``fold*``\ 函数在 Haskell 中十分重要，许多内置函数都是在\ ``fold*``\ 函数的基础上定义的，如\ :hs:func:`sum`\ 函数、\ :hs:func:`product`\ 函数、\ :hs:func:`length`\ 函数等；

  .. code-block::

     sum' :: Num a => [a] -> a
     sum' = foldl (+) 0

     product' :: Num a => [a] -> a
     product' = foldl (*) 1

     elem' :: Eq a => a -> [a] -> Bool
     elem' y = foldl (\acc x -> (x == y) || acc) False
     -- elem' 2 [1,2,3,4]
     -- (2 == 1) || False -> False (new starting value)
     --      [1,2,3,4]
     -- (2 == 2) || False -> True
     --      [2,3,4]
     -- (2 == 3) || True  -> True
     --      [3,4]
     -- (2 == 4) || True  -> True
     --      [4]

.. rubric:: 函数

.. hs:module:: Data.Foldable
   :synopsis: 该类型类能“折叠”至最终值。

.. hs:function:: foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b

   从序列左侧开始“折叠”。

.. hs:function:: foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

   从序列右侧开始“折叠”。

.. tip::

   以上两个函数通常推荐使用\ :hs:mod:`Data.Foldable`\ 模块中定义的\ :hs:func:`foldl'`\ 和\ :hs:func:`foldr'`\ 函数，比\ :hs:func:`foldl`\ 和\ :hs:func:`foldr`\ 函数更高效。

.. hs:function:: foldl1 :: Foldable t => (a -> a -> a) -> t a -> a

   作用与\ :hs:func:`foldl`\ 函数相同，但默认起始值为序列的第一个值。

.. hs:function:: foldr1 :: Foldable t => (a -> a -> a) -> t a -> a

   作用与\ :hs:func:`foldr`\ 函数相同，但默认起始值为序列的最后一个值。

.. attention::

   以上四个\ ``fold*``\ 函数是惰性的，在“折叠”过程中并不进行计算，而是将计算要求缓存，因此在处理大规模列表时，容易出现栈溢出的问题。

.. hs:function:: foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b

   :hs:func:`foldl`\ 函数的严格形式，不受\ :hs:func:`foldl`\ 函数栈溢出的影响。

.. hs:function:: foldr' :: Foldable t => (a -> b -> b) -> b -> t a -> b

   :hs:func:`foldr`\ 函数的严格形式，不受\ :hs:func:`foldr`\ 函数栈溢出的影响。

.. hs:function:: foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
   
   接受一个函数和一个可折叠序列，该函数接受一个值并返回一个\ :ref:`单位半群 <functor-monoid:单位半群>`\ 成员类型的值，对序列中每个元素应用该函数并将结果通过\ :hs:func:`~GHC.Base.mappend`\ 函数缩减为一个\ :ref:`单位半群 <functor-monoid:单位半群>`\ 值。

.. code-block::

   -- | 解析逆波兰式（Reverse Polish Notation）。
   --
   -- ==== __例子:__
   -- >>> solveRPN "10 20 + 3 *"
   -- 90.0
   --
   -- >>> solveRPN "10 20 1 sum ln"
   -- 3.4339871
   solveRPN :: String -> Float
   solveRPN = head . foldl foldingFunction [] . words
     where
       foldingFunction (x : y : zs) "+"   = (y + x) : zs
       foldingFunction (x : y : zs) "-"   = (y - x) : zs
       foldingFunction (x : y : zs) "*"   = (y * x) : zs
       foldingFunction (x : y : zs) "/"   = (y / x) : zs
       foldingFunction (x : y : zs) "^"   = (y ** x) : zs
       foldingFunction (x     : ys) "ln"  = log x : ys
       foldingFunction xs           "sum" = [sum xs]
       foldingFunction xs           num   = read num : xs

``scan*``\ 函数
===============

- ``scan*``\ 函数与\ ``fold*``\ 函数类似，但以列表方式返回所有中间起始值（包括最初起始值）；

  .. code-block::

     exp1 = scanl (+) 1 [1, 2, 3, 4] -- [1,2,4,7,11]
     exp2 = scanl (\acc y -> (2 == y) || acc) False [1, 2, 3, 4]
                                     -- [False,False,True,True,True]

.. rubric:: 函数

.. hs:module:: GHC.List
   :synopsis: 列表数据及其运算。

.. hs:function:: scanl :: (b -> a -> b) -> b -> [a] -> [b]

   同\ :hs:func:`~Data.Foldable.foldl`\ ，但返回所有中间起始值。

.. hs:function:: scanr :: (a -> b -> b) -> b -> [a] -> [b]

   同\ :hs:func:`~Data.Foldable.foldr`\ ，但返回所有中间起始值。

.. hs:function:: scanl1 :: (a -> a -> a) -> [a] -> [a]

   同\ :hs:func:`~Data.Foldable.foldl1`\ ，但返回所有中间起始值。

.. hs:function:: scanr1 :: (a -> a -> a) -> [a] -> [a]

   同\ :hs:func:`~Data.Foldable.foldr1`\ ，但返回所有中间起始值。
