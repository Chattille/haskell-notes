.. highlight:: haskell
   :linenothreshold: 10

==========
元组与列表
==========

元组
====

元组定义
--------

.. code-block::

   (<element>, ...)

- :tr:`元组 (tuple)`\ ：储存一系列不同类型元素的序列，只包含两个元素的元组称为\ :tr:`序对 (pair)`\ ；
- 语法格式：

  - 使用小括号\ ``()``\ 包围；
  - 多个元素间用逗号\ ``,``\ 分隔；
  - 逗号后的空格可省略；

- 类型：元组的类型由其储存的\ **元素个数**\ 和各个\ **元素的类型**\ 决定；

  .. code-block::

     (1, 2) :: (Num a, Num b) => (a, b)
     (1, 2, 3) :: (Num a, Num b, Num c) => (a, b, c)
     ("one", 2) :: Num b => ([Char], b)
     (1, "two") :: Num a => (a, [Char])

- Haskell 中有三元组、四元组等，但不应使用除序对外的元组，因为处理两个以上不同类型的元素有更好的办法；

  .. code-block::

     sumPair :: (Int, Int) -> Int
     sumPair (x, y) = x + y

     exp1 = sumPair (2, 3) -- 求值为 2 + 3。返回 5。

元组访问
--------

.. code-block::

   fst :: (a, b) -> a
   snd :: (a, b) -> b

- ``fst``\ 函数接受一个序对，返回序对的第一个元素；

  .. code-block::

     exp1 = fst ("alice", "liddell") -- "alice"

- ``snd``\ 函数接受一个序对，返回序对的第二个元素；

  .. code-block::

     exp2 = snd ("alice", "liddell") -- "liddell"

运算
----

.. code-block::

   uncurry :: (a -> b -> c) -> (a, b) -> c

- ``uncurry``\ 函数对一个序对应用一个二目运算函数并返回运算结果；

  .. code-block::

     res = uncurry (+) (3, 4) -- 7

列表
====

语法
----

.. code-block::

   [<element>, ...]

- :tr:`列表 (list)`\ ：储存一系列\ **相同类型**\ 元素的序列；

- 语法格式：

  - 使用中括号\ ``[]``\ 包围；
  - 多个元素间用逗号\ ``,``\ 分隔；
  - 逗号后的空格可省略；

- 字符串实质上是字符的列表，``String``\ 只是\ ``[Char]``\ 的缩写，因此可以对字符串使用列表函数；

  .. code-block::

     Prelude> :t "alice"
     "alice" :: [Char]

- 列表可以嵌套，但列表只能包含相同类型的元素；

  .. code-block::

     exp1 = [[1], [1, 2], [1, 2, 3]] -- 合法
     exp2 = [1, [1, 2]]              -- 非法
     exp3 = [(1, 2), (3, 4)]         -- 合法
     exp4 = [(1, 2), (3, 4, 5)]      -- 非法
     exp5 = [(1, 2), ('a', 4)]       -- 非法

列表定义
--------

.. code-block::

   <listName> = []
   <listName> = ... : <element> : [<elements>]

- 空列表：一对中括号\ ``[]``\ 定义一个空列表；
- :tr:`构造符 (cons operator)`\ ：

  - 分号\ ``:``\ 称为\ :ref:`构造器 <algebraic-data-type:定义>`\ ，用于构造列表；
  - 构造符左侧接受一个元素，右侧接受一个列表；
  - 构造符将左侧元素追加到右侧列表中的\ **第一个位置**\ ，并产生一个新列表；

    .. code-block::

       exp1 = 1 : []     -- [1]
       exp2 = 3 : 1 : [] -- [3,1]
       exp3 = [2, 3, 4] == 2 : 3 : 4 : [] -- True

  - 中括号\ ``[x,y]``\ 只是\ ``x : [y]``\ 的简写，列表本质上由单个元素逐一链接而成；

模式匹配
--------

.. code-block::

   <functionName> (x : xs) = <functionBody>

- ``(x : xs)``\ ：表示传入列表的第一个元素\ ``x``\ 和剩余列表\ ``xs``\ ；

.. code-block::

   doubleList :: [Integer] -> [Integer]
   doubleList []                = []
   doubleList (fstElm : rstLst) = fstElm * 2 : doubleList rstLst

   exp1 = doubleList [1, 2, 3]
   -- '1 : [2, 3]' 匹配 '(fstElm : rstLst)'
   -- 求值为 '1 * 2 : doubleList [2, 3]'
      -- '2 : [3]' 匹配 '(fstElm : rstLst)'
      -- 求值为 '2 * 2 : doublelist [3]'
         -- '3 : []' 匹配 '(fstElm : rstLst)'
         -- 求值为 '3 * 2 : doubleList []'
            -- '[]' 匹配 '[]'
            -- 求值为 '[]'
            -- 返回 '[]'
         -- 返回 '3 * 2 : []'
      -- 返回 '2 * 2 : [6]'
   -- 返回 '1 * 2 : [4, 6]'
   -- exp1 == [2, 4, 6]

范围
----

.. code-block::

   [s, a .. b]

- ``a .. b``\ ：

  - 列表元素生成范围为从\ ``a``\ 到\ ``b``\ ；

    .. code-block::

       exp1 = [1 .. 10] -- [1,2,3,4,5,6,7,8,9,10]

  - ``a``\ 和\ ``b``\ 可以为数值或字符；

    .. code-block::

       exp2 = ['a' .. 'd'] -- "abcd"

  - 当不存在\ ``s``\ 、且\ ``a``\ 大于\ ``b``\ 时返回空列表；
  - 当不存在\ ``b``\ 时，列表无限长（因为 Haskell 为惰性的，所以只有用到的元素会生成）；

    .. code-block::

       exp3 = take 5 [2, 4 ..] -- [2,4,6,8,10]

  - 尽量不使用浮点数，因为存在精度问题；

    .. code-block::

       exp4 = [0.1, 0.2 .. 0.5] -- [0.1,0.2,0.30000000000000004,0.4,0.5]

- ``s``\ ：列表头两个值的差值为该范围的步长；

  .. code-block::

     exp5 = [2, 4 .. 20] -- [2,4,6,8,10,12,14,16,18,20]
     exp6 = [10, 9 .. 1] -- [10,9,8,7,6,5,4,3,2,1]

.. hs:module:: GHC.List

列表访问
--------

.. hs:function:: (!!) :: [a] -> Int -> a

   接受一个列表和一个索引值，返回对应索引的元素。

   索引值从 0 开始。若索引值小于 0 则报错（\ ``negative index``\ ），大于列表长度则报错（\ ``index too large``\ ）。

   .. code-block::

      a :: [Integer]
      a = [1, 4, 2, 3]
      exp1 = a !! 2 -- 2
      exp2 = a !! 0 -- 1

.. hs:function:: head :: [a] -> a

   接受一个非空列表，返回列表第一个元素。

   .. code-block::

      exp3 = head a -- 1

.. hs:function:: last :: [a] -> a

   接受一个非空列表，返回列表最后一个元素。

   .. code-block::

      exp4 = last a -- 3

.. hs:function:: tail :: [a] -> [a]

   接受一个非空列表，返回列表除第一个元素的剩余元素列表。

   .. code-block::

      exp5 = tail a -- [4,2,3]

.. hs:function:: init :: [a] -> [a]

   接受一个非空列表，返回列表除最后一个元素的剩余元素列表。

   .. code-block::

      exp6 = init a -- [1,4,2]

.. hs:function:: take :: Int -> [a] -> [a]

   接受一个整数\ ``n``\ 和一个列表，返回列表前\ ``n``\ 个元素组成的新列表。

   若整数\ ``n``\ 小于等于 0 则返回空列表，大于列表长度则返回整个列表。

   .. code-block::

      exp7  = take 2 a    -- [1,4]
      exp8  = a           -- [1,4,2,3]
      exp9  = take (-1) a -- []
      exp10 = take 10 a   -- [1,4,2,3]

.. hs:function:: drop :: Int -> [a] -> [a]

   接受一个整数\ ``n``\ 和一个列表，返回除列表前\ ``n``\ 个元素外剩余元素组成的新列表。

   若整数\ ``n``\ 小于等于 0 则返回整个列表，大于列表长度则返回空列表。

   .. code-block::

      exp11 = drop 1 a    -- [4,2,3]
      exp12 = drop 3 a    -- [3]
      exp13 = drop (-1) a -- [1,4,2,3]
      exp14 = drop 10 a   -- []

.. hs:module:: GHC.Base

合并
----

.. hs:function:: (++) :: [a] -> [a] -> [a]

   接受两个列表，并将后一个列表追加在前一个列表后，返回一个新列表。

   .. code-block::

      exp1 = [1, 2, 3] ++ [2, 3, 4] -- [1,2,3,2,3,4]
      exp2 = "alice" ++ "liddell"   -- "aliceliddell"
      exp3 = [] ++ []               -- []

.. hs:module:: GHC.List

.. hs:function:: zip :: [a] -> [b] -> [(a, b)]

   接受两个列表，返回储存一系列序对的新列表，每对序对储存对应索引值的元素，当短列表用尽时，函数执行完毕。

   有对应函数\ :hs:func:`zip3`\ ，用于合并三个列表。

   .. code-block::

      exp4 = zip [4, 5, 2] "alice" -- [(4,'a'),(5,'l'),(2,'i')]

.. hs:function:: zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

   接受一个函数和两个列表，对两个列表进行遍历，并将两个列表的元素传入函数，计算结果，最终返回结果列表。

   有对应函数\ :hs:func:`zipWith3`\ ，用于合并三个列表。

   .. code-block::

      exp5 = zipWith (+) [1, 2, 3] [4, 5, 6] -- [5,7,9]
      exp6 = zipWith (zipWith (*)) [[1], [1, 2]] [[3], [3, 4]] -- [[3],[3,8]]

拆分
----

.. hs:function:: splitAt :: Int -> [a] -> ([a], [a])

   接受一个索引值\ ``n``\ 和一个列表，返回从索引处拆分后的两个新列表组成的序对。

   索引值从 0 开始。若索引值小于等于 0，则序对第一个元素为空元素，若大于等于列表长度，则序对第二个元素为空元素。

   .. code-block::

      a :: String
      a = "liddell"
      exp1 = splitAt 0 a  -- ("","liddell")
      exp2 = splitAt 3 a  -- ("lid","dell")
      exp3 = splitAt 10 a -- ("liddell","")

.. hs:function:: lines :: String -> [String]
   :module: Data.OldList

   接受一个字符串，按换行符拆分字符串并返回列表。

   有对应函数\ :hs:func:`unlines`\ ，接受一个字符串列表，在每个字符串元素后添加换行符后合并为一个新字符串。

   .. code-block::

      exp4 = lines "alice\nliddell" -- ["alice","liddell"]
      exp5 = unlines ["foo", "bar"] -- "foo\nbar\n"

.. hs:function:: words :: String -> [String]
   :module: Data.OldList

   接受一个字符串，按一或多个连续空白字符拆分字符串并返回列表。

   有对应函数\ :hs:func:`unwords`\ ，接受一个字符串列表，用一个空格符链接所有元素。

   .. code-block::

      exp6 = words "quick  \r  brown\n  fox"   -- ["quick","brown","fox"]
      exp7 = unwords ["quick", "brown", "fox"] -- "quick brown fox"

排序
----

.. hs:function:: reverse :: [a] -> [a]

   接受一个列表，返回倒序排序的新列表。

   .. code-block::

      a :: String
      a = "alice"
      exp1 = reverse a -- "ecila"

.. hs:module:: Data.Foldable

统计
----

.. hs:function:: length :: Foldable t => t a -> Int

   接受一个序列，返回序列的元素个数。

   .. code-block::

      exp1 = length "alice" -- 5

.. hs:function:: maximum :: (Foldable t, Ord a) => t a -> a

   接受一个序列，返回序列中的最大元素。

   .. code-block::

      exp2 = maximum "alice" -- 'l'

.. hs:function:: minimum :: (Foldable t, Ord a) => t a -> a

   接受一个序列，返回序列中的最小元素。

   .. code-block::

      exp3 = minimum "alice" -- 'a'

.. hs:function:: sum :: (Foldable t, Num a) => t a -> a

   接受一个数值列表，返回所有元素之和。

   .. code-block::

      exp4 = sum [8, 4, 10, 4, 8, 2] -- 36

.. hs:function:: product :: (Foldable t, Num a) => t a -> a

   接受一个数值列表，返回所有元素之积。

   .. code-block::

      exp5 = product [2, 4, 6, 6, 3] -- 864

判断
----

.. hs:function:: elem :: (Foldable t, Eq a) => a -> t a -> Bool

   接受一个元素\ ``x``\ 和一个序列，若元素\ ``x``\ 存在于序列中，则返回\ ``True``\ 。

   有对应函数\ :hs:func:`notElem`\ ，当元素\ ``x``\ 不存在于序列中时返回\ ``True``\ 。

   .. attention::

      :hs:func:`elem`\ 函数和\ :hs:func:`notElem`\ 函数在判断时会遍历序列，并用元素\ ``x``\ 依次与序列元素进行相等性比较，因此元素\ ``x``\ 的类型必须为 :ref:`Eq 类型类<type-class-eq>`\ 的成员。

   .. code-block::

      a :: [Integer]
      a = [1, 4, 2, 3]
      exp1 = 5 `elem` a -- False
      exp2 = elem 3 a   -- True

.. hs:function:: null :: Foldable t => t a -> Bool

   接受一个序列，若序列为空，则返回\ ``True``\ 。

   .. code-block::

      exp3 = null a  -- False
      exp4 = null [] -- True

.. hs:function:: and :: Foldable t => t Bool -> Bool

   接受一个布尔值列表，若所有元素为\ ``True``\ ，则返回\ ``True``\ 。

   .. code-block::

      exp5 = and [True, True, True]  -- True
      exp6 = and [True, False, True] -- False

.. hs:function:: or :: Foldable t => t Bool -> Bool

   接受一个布尔值列表，若存在一个元素为\ ``True``\ ，则返回\ ``True``\ 。

   .. code-block::

      exp7 = or [True, True, False]   -- True
      exp8 = or [False, False, False] -- False

.. hs:function:: all :: Foldable t => (a -> Bool) -> t a -> Bool

   接受一个\ :ref:`谓词 <predicate>`\ 和一个列表，若谓词对所有元素的判断均为\ ``True``\ ，则返回\ ``True``\ 。

   .. code-block::

      exp9  = all odd [1, 3, 5, 7]  -- True
      exp10 = all even [2, 3, 4, 6] -- False

.. hs:function:: any :: Foldable t => (a -> Bool) -> t a -> Bool

   接受一个\ :ref:`谓词 <predicate>`\ 和一个列表，若谓词对至少一个元素的判断为\ ``True``\ ，则返回\ ``True``\ 。

   .. code-block::

      exp11 = any odd [1, 2, 3, 4]  -- True
      exp12 = any even [1, 3, 5, 7] -- False

.. hs:module:: GHC.List

.. hs:function:: takeWhile :: (a -> Bool) -> [a] -> [a]

   接受一个\ :ref:`谓词 <predicate>`\ 和一个列表，直到谓词判断为\ ``False``\ 之前，函数将所有判断为\ ``True``\ 的元素返回为新列表。

   .. code-block::

      exp13 = takeWhile odd [1, 3, 5, 6, 7, 8, 9] -- [1,3,5]

.. hs:function:: dropWhile :: (a -> Bool) -> [a] -> [a]

   接受一个\ :ref:`谓词 <predicate>`\ 和一个列表，直到谓词判断为\ ``False``\ 之前，函数将所有判断为\ ``True``\ 的元素排除并将剩余元素返回为新列表。

   .. code-block::

      exp14 = dropWhile odd [1, 3, 5, 6, 7, 8, 9] -- [6,7,8,9]

.. hs:function:: filter :: (a -> Bool) -> [a] -> [a]

   接受一个\ :ref:`谓词 <predicate>`\ 和一个列表，返回列表中判断为\ ``True``\ 的\ **每个元素**\ 。

   .. code-block::

      exp15 = filter odd [1, 3, 5, 6, 7, 8, 9] -- [1,3,5,7,9]

.. hs:function:: span :: (a -> Bool) -> [a] -> ([a], [a])

   接受一个\ :ref:`谓词 <predicate>`\ 和一个列表，返回一个序对，序对第一个元素为直到谓词判断为\ ``False``\ 之前判断为\ ``True``\ 的元素列表，第二个元素为剩余元素列表。

   .. code-block::

      exp16 = span odd [1, 3, 5, 6, 7, 8, 9] -- ([1,3,5],[6,7,8,9])

.. hs:function:: break :: (a -> Bool) -> [a] -> ([a], [a])

   接受一个\ :ref:`谓词 <predicate>`\ 和一个列表，返回一个序对，序对第一个元素为直到谓词判断为\ ``True``\ 之前判断为\ ``False``\ 的元素列表，第二个元素为剩余元素列表。

   .. code-block::

      exp17 = break even [1, 3, 5, 6, 7, 8, 9] -- ([1,3,5],[6,7,8,9])

.. hs:module:: Data.Foldable

其他
----

.. hs:function:: concat :: Foldable t => t [a] -> [a]

   接受一个元素为列表的列表，去掉一层列表并返回结果。

   .. code-block::

      exp1 = concat [[1], [1, 2], [1, 2, 3]] -- [1,1,2,1,2,3]

.. hs:function:: concatMap :: Foldable t => (a -> [b]) -> t a -> [b]

   接受一个函数和一个列表，对列表中每个元素应用函数，合成新列表后去掉一层列表。

   .. code-block::

      exp2 = concatMap (replicate 3) [1..3] -- [1,1,1,2,2,2,3,3,3]

.. hs:module:: GHC.List

.. hs:function:: cycle :: [a] -> [a]

   接受一个列表，并无限循环该列表。

   .. code-block::

      exp3 = take 10 (cycle [1, 2, 3]) -- [1,2,3,1,2,3,1,2,3,1]

.. hs:function:: repeat :: a -> [a]

   接受一个元素，并无限循环该元素。

   .. code-block::

      exp4 = take 10 (repeat 5) -- [5,5,5,5,5,5,5,5,5,5]

.. hs:function:: replicate :: Int -> a -> [a]

   接受一个整数\ ``n``\ 和一个元素，并循环该元素\ ``n``\ 次。

   .. code-block::

      exp5 = replicate 5 'a' -- "aaaaa"

.. hs:function:: iterate :: (a -> a) -> a -> [a]

   接受一个函数和一个起始值，函数对起始值应用函数，得到结果并对结果应用函数，依此类推，返回无限长的列表，包含所有结果。

   .. code-block::

      exp6 = take 10 (iterate (* 2) 1) -- [1,2,4,8,16,32,64,128,256,512]

.. hs:module:: GHC.Base

.. hs:function:: map :: (a -> b) -> [a] -> [b]

   接受一个函数和一个列表，遍历列表并对各个元素应用函数，返回结果列表。

   .. code-block::

      exp7 = map (+ 1) [1 .. 5] -- [2,3,4,5,6]

列表推导式
==========

.. code-block::

   [<output> | <var> <- <input>, ..., <predicate>, ...]

- 来源：推导式来源于数学中的集合推导式，如 :math:`S = \{2 \cdot x | x \in \mathbb{N}, x \le 10\}` 表示集合 :math:`\{0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20\}`\ ，其中 :math:`2 \cdot x` 为\ :tr:`输出函数 (output)`\ ，\ :math:`x` 为变量，:math:`x \in \mathbb{N}` 为\ :tr:`输入集合 (input set)`\ ，\ :math:`x \le 10` 为条件或\ :tr:`谓词 (predicate)`\ ；
- 语法格式：

  - 输出函数：对每个元素要做的操作，可使用\ :ref:`表达式 <expression:\`\`if\`\`\\ 表达式>`\ ，可嵌套；
  - ``|``\ ：分隔输出函数部分和剩余部分；
  - 变量：指定与序列个元素相绑定的变量名，可使用下划线\ ``_``\ ；
  - ``<-``\ ：遍历序列，并将变量与序列中的各个元素进行绑定；
  - 输入：变量的数据来源，多个输入用逗号\ ``,``\ 分隔；
  - 谓词：条件判断，丢弃谓词不成立的元素，和守卫语法基本一致；

.. code-block::

   exp1 = [ x * 2 | x <- [0 .. 10] ] -- [0,2,4,6,8,10,12,14,16,18,20]
   exp2 = [ x * 2 | x <- [0 .. 10], x * 2 >= 12 ] -- [12,14,16,18,20]
   exp3 = [ if x < 10 then "FOO" else "BAR" | x <- [7 .. 13], odd x ]
                                -- ["FOO","FOO","BAR","BAR"]
   exp4 = [ x | x <- [10 .. 20], x /= 13, x /= 15, x /= 19 ]
                                -- [10,11,12,14,16,17,18,20]
   exp5 = [ x * y | x <- [2, 5, 10], y <- [8, 10, 11], x * y > 50 ]
                                -- [55,80,100,110]
   keepUpper :: String -> String
   keepUpper st = [ x | x <- st, x `elem` ['A' .. 'Z'] ]

   exp6 = keepUpper "AliceInWonderland" -- "AIW"

   xxs :: [[Int]]
   xxs =
       [ [1, 3, 5, 2, 3, 1, 2]
       , [1, 2, 3, 4, 5, 6, 7]
       , [1, 2, 4, 2, 1, 6, 3, 1, 3, 2]
       ]

   exp7 = [ [ x | x <- xs, even x ] | xs <- xxs ]
                                -- [[2,2],[2,4,6],[2,4,2,6,2]]
   exp8 = -- brittany 风格
       [ (a, b, c)
       | c <- [1 .. 10]
       , b <- [1 .. c]
       , a <- [1 .. b]
       , a + b + c == 24
       , a ^ 2 + b ^ 2 == c ^ 2
       ]                        -- [(6,8,10)]
