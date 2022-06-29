.. highlight:: haskell
   :linenothreshold: 10

====
模块
====

简介
====

.. code-block::

   import <module> (<function>, ...)
   import <module> hiding (<function>, ...)
   import qualified <module>
   import <module> as <alias>

- :tr:`模块 (module)`\ ：一组相关函数、类和类型类，一个 Haskell 程序就是主模块加载其他模块的过程；
- 导入：\ ``import``\ 关键字导入\ **同一目录**\ 下的模块，导入模块后，模块包含的函数、类和类型类可直接使用；

  .. code-block::

     import Data.List

     numUniques :: Eq a => [a] -> Int
     numUniques = length . nub

- 部分导入：小括号\ ``()``\ 包括要导入的函数、类或类型类；

  .. code-block::

     import Data.List (nub, sort)

- 反选：\ ``hiding``\ 关键字可将指定函数、类或类型类排除导入范围；

  .. code-block::

     import Data.List hiding (nub) -- 导入除 'nub' 外的所有元素

- 命名冲突：\ ``qualified``\ 关键字指定导入的模块中重名函数、类或类型类前必须添加模块名；

  .. code-block::

     import qualified Data.Map

     test1 :: [Integer] -- 来自 "Prelude"
     test1 = filter odd [1, 2, 3, 4]

     test2 :: Data.Map.Map Integer [Char] -- 来自 "Data.Map"
     test2 =
         Data.Map.filter (> "a") (Data.Map.fromList [(1, "a"), (2, "b")])

- 别名：\ ``as``\ 关键字为模块起别名；

  .. code-block::

     import qualified Data.Map as M
     res :: M.Map Integer [Char]
     res = M.filter (> "a") (M.fromList [(1, "a"), (2, "b")])

- GHCi：\ ``:module [+/-] [*]<mod> ...``\ 可在 GHCi 中导入或移除相应模块，多个模块用空格分隔，若省略符号则清除除\ :hs:mod:`Prelude`\ 模块的所有模块，并切换至指定模块；

  .. code-block::

     Prelude> :m + Data.List
     Prelude Data.List> :t nub
     nub :: Eq a => [a] -> [a]
     Prelude Data.List> :m - Data.List
     Prelude> :t nub

     <interactive>:1:1: error: Variable not in scope: nub

导出模块
========

.. code-block::

   module <module>.<submodule>
       ( <function>
       , <algebraicDataType>(<valueConstructor, ...)
       , module <module>
       , ...
       ) where

- 导出：将当前模块中的函数、类型或类型类导出，使得外部只能使用导出的函数、类型或类型类，未导出的不能从外部使用；
- 语法格式：

  - ``module``\ 关键字定义当前模块名；

    - 模块名应与模块文件名相同；
    - 可指定子模块，所有子模块位于同一目录（模块）下；

  - 小括号\ ``()``\ 内定义导入当前模块时会被导出的函数、类型或类型类，多个函数、类型或类型类用逗号分隔；

    - 导出\ :doc:`代数数据类型 <algebraic-data-type>`\ 时，可在其后指定要导出的值构造器，多个值构造器用逗号分隔；
    - 若要导出所有值构造器，则可使用\ ``..``\ ；

      .. code-block::

         module Shapes
             ( Point(..) -- 从代数类型 'Point' 导入所有构造器
             , Shape(Circle, Rectangle) -- 仅导入两个构造器
             , surface   -- 导入函数
             ) where

    - 若要导出模块，则用\ ``module``\ 关键字指定模块名；

  - ``where``\ 关键字表示模块的起始；

.. tabs::

   .. tab:: Geometry/Sphere.hs

      .. code-block::

         module Geometry.Sphere
             ( volume
             , area
             ) where

         volume :: Float -> Float
         volume radius = (4.0 / 3.0) * pi * (radius ^ 3)

         area :: Float -> Float
         area radius = 4 * pi * (radius ^ 2)

   .. tab:: Geometry/Cuboid.hs

      .. code-block::

         module Geometry.Cuboid
             ( volume
             , area
             ) where

         volume :: Float -> Float -> Float -> Float
         volume a b c = rectangleArea a b * c

         area :: Float -> Float -> Float -> Float  
         area a b c =
             rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

         rectangleArea :: Float -> Float -> Float -- 未导出
         rectangleArea a b = a * b

   .. tab:: Geometry/Cube.hs

      .. code-block::

         module Geometry.Cube
             ( volume
             , area
             ) where

         import qualified Geometry.Cuboid as Cuboid

         volume :: Float -> Float
         volume side = Cuboid.volume side side side

         area :: Float -> Float
         area side = Cuboid.area side side side

官方模块
========

- 所有官方模块及其子模块、函数、类和类型类的签名和源代码均可在\ 官网_\ 查看；

``Prelude``
-----------

.. hs:module:: Prelude

- :hs:mod:`Prelude`\ 模块默认向每个 Haskell 程序导入多个标准函数、类和类型类；
- :hs:mod:`Prelude`\ 是进入 GHCi 时的默认模块，不可移除；

``Data.List``
-------------

.. hs:module:: Data.List

- :hs:mod:`Data.List`\ 模块提供关于列表操作的函数；
- :hs:mod:`Prelude`\ 模块导入了部分该模块函数；

插入
~~~~

.. hs:function:: intersperse :: a -> [a] -> [a]

   接受一个元素和一个列表，将该元素插入列表中每两个元素之间。

   .. code-block::

      import Data.List
      exp1 = intersperse 0 [1, 2, 3, 4] -- [1,0,2,0,3,0,4]

.. hs:function:: intercalate :: [a] -> [[a]] -> [a]

   接受一个列表和一个包含列表的列表，将该列表插入列表的列表中每两个列表之间。

   .. code-block::

      exp2 = intercalate [0, 0] [[1, 2], [3, 4]] -- [1,2,0,0,3,4]

.. hs:function:: insert :: Ord a => a -> [a] -> [a]

   接受一个元素和一个列表，从列表第一个元素开始，若小于等于下一个元素，则在当前元素后下一元素前插入该元素。

   .. code-block::

      exp3 = insert 4 [3, 5, 1, 2, 8, 2] -- [3,4,5,1,2,8,2]
      exp4 = insert 4 [1, 3, 4, 4, 1]    -- [1,3,4,4,4,1]

删除
~~~~

.. hs:function:: delete :: Eq a => a -> [a] -> [a]

   接受一个元素和一个列表，移除第一个匹配。

   .. code-block::

      exp5 = delete 'h' "hey there" -- "ey there"

重构
~~~~

.. hs:function:: transpose :: [[a]] -> [[a]]

   接受一个包含列表的列表，并用每个子列表对应索引的元素组成新的子列表（可将列表的列表看作矩阵，\ :hs:func:`transpose`\ 函数转换矩阵的行和列）。

   .. code-block::

      exp6 = transpose [[1, 2, 3], [4, 5, 6]] -- [[1,4],[2,5],[3,6]]

.. hs:function:: sort :: Ord a => [a] -> [a]

   对列表升序排序。

   .. code-block::

      exp7 = sort [1, 3, 2, 6, 5]       -- [1,2,3,5,6]
      exp8 = sort "Alice in Wonderland" -- "  AWacddeeiillnnnor"

.. hs:function:: group :: Eq a => [a] -> [[a]]

   接受一个列表，若相邻元素相同则组成子列表。

   .. code-block::

      exp9 = group [1, 1, 1, 2, 2, 3, 3, 3, 3] -- [[1,1,1],[2,2],[3,3,3,3]]

.. hs:function:: inits :: [a] -> [[a]]

   与\ :hs:func:`~GHC.List.init`\ 函数类似，但会对结果迭代应用。

   .. code-block::

      exp10 = inits [1, 2, 3] -- [[],[1],[1,2],[1,2,3]]

.. hs:function:: tails :: [a] -> [[a]]

   与\ :hs:func:`~GHC.List.tail`\ 函数类似，但会对结果迭代应用。

   .. code-block::

      exp11 = tails [1, 2, 3] -- [[1,2,3],[2,3],[3],[]]

      searchSublist :: Eq a => [a] -> [a] -> Bool
      searchSublist needle haystack =
          let nlen   = length needle
              isPart = \b x -> (take nlen x == needle) || b
          in  foldl isPart False (tails haystack)

.. hs:function:: partition :: (a -> Bool) -> [a] -> ([a], [a])

   接受一个谓词和一个列表，返回一个序对，第一个元素为所有符合谓词的元素的列表，第二个为不符合的列表。

   .. code-block::

      exp12 = partition (`elem` ['A' .. 'Z']) "ALICEmaryAMBERcindy"
              -- ("ALICEAMBER","marycindy")

迭代
~~~~

- :hs:func:`~GHC.List.iterate`\ 函数详见\ :ref:`列表的其他操作 <tuple-and-list:其他>`\ ；
- ``fold*``\ 和\ ``scan*``\ 函数详见\ ``fold``\ :ref:`递归模式 <recursion:\`\`fold\`\`\\ 模式>`\ ；

判断
~~~~

.. hs:function:: isInfixOf :: Eq a => [a] -> [a] -> Bool

   接受两个列表，若第二个列表包含第一个列表，则返回\ ``True``\ 。

   .. code-block::

      exp13 = "alice" `isInfixOf` "alice in wonderland" -- True

.. hs:function:: isPrefixOf :: Eq a => [a] -> [a] -> Bool

   接受两个列表，若第二个列表开头包含第一个列表，则返回\ ``True``\ 。

   .. code-block::

      exp14 = isPrefixOf "hey" "oh hey there" -- False

.. hs:function:: isSuffixOf :: Eq a => [a] -> [a] -> Bool

   接受两个列表，若第二个列表结尾包含第一个列表，则返回\ ``True``\ 。

   .. code-block::

      exp15 = "there" `isSuffixOf` "oh there hey" -- False

查找
~~~~

.. hs:function:: find :: Foldable t => (a -> Bool) -> t a -> Maybe a

   接受一个谓词和一个列表，以\ ``Maybe``\ 类型返回第一个符合谓词的元素。

   .. code-block::

      exp16 = find (> 3) [1 .. 5] -- Just 4
      exp17 = find (> 9) [1 .. 5] -- Nothing

.. hs:function:: findIndex :: (a -> Bool) -> [a] -> Maybe Int

   与\ :hs:func:`find`\ 函数类似，但以\ ``Maybe``\ 类型返回第一个符合谓词的元素索引。

   .. code-block::

      exp18 = findIndex (== 4) [5, 3, 2, 1, 6, 4] -- Just 5

.. hs:function:: findIndices :: (a -> Bool) -> [a] -> [Int]

   与\ :hs:func:`findIndex`\ 函数类似，但返回所有符合谓词的索引。

   .. code-block::

      exp19 = findIndices (`elem` ['A' .. 'Z']) "Where Are The Caps?"
              -- [0,6,10,14]

.. hs:function:: elemIndex :: Eq a => a -> [a] -> Maybe Int

   与\ :hs:func:`~Data.Foldable.elem`\ 函数类似，以\ ``Maybe``\ 类型返回该元素第一个匹配的索引。

   .. code-block::

      exp20 = 4 `elemIndex` [1 .. 5] -- Just 3
      exp21 = 9 `elemIndex` [1 .. 5] -- Nothing

.. hs:function:: elemIndices :: Eq a => a -> [a] -> [Int]

   与\ :hs:func:`elemIndex`\ 函数类似，但返回所有匹配索引的列表。

   .. code-block::

      exp22 = 4 `elemIndices` [1, 4, 2, 4] -- [1,3]
      exp23 = 9 `elemIndices` [1, 4, 2, 4] -- []

合并
~~~~

.. hs:function:: zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]

   与\ :hs:func:`~GHC.List.zip`\ 、\ :hs:func:`~GHC.List.zip3`\ 函数相似，合并对应数字数量的列表，最大为\ :hs:func:`~base-4.14.3.0:Data.OldList.zip7`\ 。

.. hs:function:: zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]

   与\ :hs:func:`~GHC.List.zipWith`\ 、\ :hs:func:`~GHC.List.zipWith3`\ 函数相似，合并对应数字数量的列表，最大为\ :hs:func:`~base-4.14.3.0:Data.OldList.zipWith7`\ 。

拆分
~~~~

- :hs:func:`~GHC.List.splitAt`\ 、\ :hs:func:`~base-4.14.3.0:Data.OldList.lines`\ 、\ :hs:func:`~base-4.14.3.0:Data.OldList.unlines`\ 、\ :hs:func:`~base-4.14.3.0:Data.OldList.words`\ 和\ :hs:func:`~base-4.14.3.0:Data.OldList.unwords`\ 函数详见\ :ref:`列表拆分 <tuple-and-list:拆分>`；

集合
~~~~

.. hs:function:: nub :: Eq a => [a] -> [a]

   接受一个列表，移除所有重复元素。

   .. code-block::

      exp24 = nub [1, 2, 2, 3, 1, 3, 4, 4] -- [1,2,3,4]

.. hs:function:: (\\) :: Eq a => [a] -> [a] -> [a]

   对两个列表进行差集运算，移除第一个列表中与第二个列表元素相同的元素。

   .. code-block::

      exp25 = [1 .. 10] \\ [1, 3, 6, 8, 9] -- [2,4,5,7,10]

.. hs:function:: union :: Eq a => [a] -> [a] -> [a]

   对两个列表进行合集运算，若第二个列表中的元素在第一个中不存在，则追加到第一个列表的末尾。

   .. code-block::

      exp26 = "alice" `union` "alice in wonderland" -- "alice nwodr"

.. hs:function:: intersect :: Eq a => [a] -> [a] -> [a]

   对两个列表进行交集运算。

   .. code-block::

      exp27 = [1 .. 7] `intersect` [5 .. 10] -- [5,6,7]

泛用
~~~~

.. hs:function:: genericLength :: Num i => [a] -> i

.. hs:function:: genericTake :: Integral i => i -> [a] -> [a]

.. hs:function:: genericDrop :: Integral i => i -> [a] -> [a]

.. hs:function:: genericSplitAt :: Integral i => i -> [a] -> ([a], [a])

.. hs:function:: genericIndex :: Integral i => [a] -> i -> a

.. hs:function:: genericReplicate :: Integral i => i -> a -> [a]

   对应函数的泛用版，原函数只接受或返回\ ``Int``\ 类型，而泛用版接受或返回更宽泛的类型（如\ ``Integral``\ 或\ ``Num``\ ）。

   .. code-block::

      exp28 = let xs = [1 .. 10] in sum xs / genericLength xs -- 5.5

.. hs:function:: nubBy :: (a -> a -> Bool) -> [a] -> [a]

.. hs:function:: deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]

.. hs:function:: unionBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]

.. hs:function:: intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]

.. hs:function:: groupBy :: (a -> a -> Bool) -> [a] -> [[a]]

   对应函数的泛用版，原函数使用\ :hs:func:`(==)`\ 进行相等性比较，而泛用版用相等性函数比较，通常和\ :hs:func:`Data.Function.on`\ 函数结合使用。

   .. code-block::

      exp29 = let xs = [-3, -2, 5, 4, -6, 5, 7]
              in  groupBy (\x y -> (x > 0) == (y > 0)) xs
              -- [[-3,-2],[5,4],[-6],[5,7]]

.. hs:function:: sortBy :: (a -> a -> Ordering) -> [a] -> [a]

.. hs:function:: insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]

.. hs:function:: maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a

.. hs:function:: minimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a

   对应函数的泛用版，原函数使用\ :hs:func:`compare`\ 函数进行大小比较，而泛用版用大小比较函数比较，通常和\ :hs:func:`Data.Function.on`\ 函数结合使用。

   .. code-block::

      import Data.Function (on)
      exp30 = let xs = [[5, 4, 5, 4, 4], [1, 2, 3], [3, 5, 4, 3], [], [2], [2, 2]]
              in  sortBy (compare `on` length) xs
              -- [[],[2],[2,2],[1,2,3],[3,5,4,3],[5,4,5,4,4]]

``Data.Char``
-------------

.. hs:module:: Data.Char
   :synopsis: 处理字符或字符串的函数和类型类。

字符判断
~~~~~~~~

.. hs:function:: isControl :: Char -> Bool

   判断字符是否为 Unicode 控制字符。

   .. code-block::

      exp1 = isControl '\n' -- True

.. hs:function:: isSpace :: Char -> Bool

   判断字符是否为 Unicode 空白字符（空格、换行、回车、水平制表符、垂直制表符）。

   .. code-block::

      exp2 = isSpace '\r' -- True

.. hs:function:: isLower :: Char -> Bool

   判断字符是否为 Unicode 小写字母。

   .. code-block::

      exp3 = isLower 'ф' -- True

.. hs:function:: isUpper :: Char -> Bool

   判断字符是否为 Unicode 大写字母。

   .. code-block::

      exp4 = isUpper 'D' -- True

.. hs:function:: isLetter :: Char -> Bool

.. hs:function:: isAlpha :: Char -> Bool

   判断字符是否为 Unicode 字母。

   .. code-block::

      exp5 = isAlpha 'ה' -- True

.. hs:function:: isAlphaNum :: Char -> Bool

   判断字符是否为 Unicode 字母或 Unicode 数字。

   .. code-block::

      exp6 = isAlphaNum '三' -- True
      exp7 = isAlphaNum 'i'  -- True

.. hs:function:: isPrint :: Char -> Bool

   判断字符是否为 Unicode 可打印字符。

   .. code-block::

      exp8 = isPrint ' ' -- True

.. hs:function:: isDigit :: Char -> Bool

   判断字符是否为 ASCII 数字。

   .. code-block::

      exp9  = isDigit 'i' -- False
      exp10 = isDigit '0' -- True

.. hs:function:: isOctDigit :: Char -> Bool

   判断字符是否为 ASCII 八进制数字。

   .. code-block::

      exp11 = isOctDigit '8' -- False

.. hs:function:: isHexDigit :: Char -> Bool

   判断字符是否为 ASCII 十六进制数字。

   .. code-block::

      exp12 = isHexDigit 'E' -- True

.. hs:function:: isMark :: Char -> Bool

   判断字符是否为 Unicode 标记字母（与前一个字符结合形成带符号字母）。

   .. code-block::

      exp13 = isMark '́'      -- True
      exp14 = map isMark "á" -- [False,True]

.. hs:function:: isNumber :: Char -> Bool

   判断字符是否为 Unicode 数字字符。

   .. code-block::

      exp15 = isNumber 'i' -- False
      exp16 = isNumber 'Ⅸ' -- True

.. hs:function:: isPunctuation :: Char -> Bool

   判断字符是否为 Unicode 标点符号。

   .. code-block::

      exp17 = isPunctuation '。' -- True

.. hs:function:: isSymbol :: Char -> Bool

   判断字符是否为 Unicode 货币或数学符号。

   .. code-block::

      exp18 = isSymbol '△' -- True
      exp19 = isSymbol '-' -- False

.. hs:function:: isSeparator :: Char -> Bool

   判断字符是否为 Unicode 空格或 Unicode 分隔符。

   .. code-block::

      exp20 = isSeparator '\160' -- True

.. hs:function:: isAscii :: Char -> Bool

   判断字符是否为 ASCII 字符。

   .. code-block::

      exp21 = isAscii '\a' -- True

.. hs:function:: isLatin1 :: Char -> Bool

   判断字符是否为 Latin1 (ISO 8859-1) 字符。

   .. code-block::

      exp22 = isLatin1 'á' -- True

.. hs:function:: isAsciiLower :: Char -> Bool

   判断字符是否为 ASCII 小写字母。

.. hs:function:: isAsciiUpper :: Char -> Bool

   判断字符是否为 ASCII 大写字母。

类型
~~~~

.. hs:function:: generalCategory :: Char -> GeneralCategory

   返回字符的一般类型\ ``GeneralCategory``\ 。

   ``GeneralCategory``\ 类型包含31种数据，本身是\ ``Eq``\ 类型类的成员。

   .. code-block::

      exp1 = generalCategory ' '  -- Space
      exp2 = generalCategory 'A'  -- UppercaseLetter
      exp3 = generalCategory 'a'  -- LowercaseLetter
      exp4 = generalCategory '\n' -- Control
      exp5 = generalCategory '1'  -- DecimalNumber

转换
~~~~

.. hs:function:: toUpper :: Char -> Char

   将字符转换为相应 Unicode 大写字母，非大小写字母不变。

   .. code-block::

      exp1 = toUpper 'ф' -- '\1060'

.. hs:function:: toLower :: Char -> Char

   将字符转换为相应 Unicode 小写字母，非大小写字母不变。

   .. code-block::

      exp2 = toLower '\1060' -- '\1092'

.. hs:function:: toTitle :: Char -> Char

   将字符转换为相应 Unicode 标题大写字母或 Unicode 大写字母，对于非连写字母，标题大写字母与大写字母一致。

   .. code-block::

      exp3 = toTitle 'f' -- 'F'

.. hs:function:: digitToInt :: Char -> Int

   将字符转换为对应`Int`数字，该字符必须满足`isHexDigit`，否则报错。

   .. code-block::

      exp4 = digitToInt 'a' -- 10

.. hs:function:: intToDigit :: Int -> Char

   将 0 到 15 闭区间的\ ``Int``\ 数字转换为字符。

   .. code-block::

      exp5 = intToDigit 12 -- 'c'

.. hs:function:: ord :: Char -> Int

   将 Unicode 字符转换为 Unicode 码。

   .. code-block::

      exp6 = ord 'a' -- 97
      exp7 = ord '三' -- 19977

.. hs:function:: chr :: Int -> Char

   将 Unicode 码转换为 Unicode 字符。

   .. code-block::

      exp8 = chr 120 -- 'x'

      -- | 凯撒密码加密。
      encode :: Int -> String -> String
      encode shift msg = map (chr . (+ shift) . ord) msg
      exp9 = encode 3 "Alice Liddell" -- "Dolfh#Olgghoo"

      -- | 凯撒密码解密。
      decode :: Int -> String -> String
      decode shift msg = encode (negate shift) msg
      exp10 = decode 3 "Dolfh#Olgghoo" -- "Alice Liddell"

``Data.Map``
------------

- :hs:mod:`Data.Map`\ 模块提供处理\ :doc:`映射 <map>`\ 的函数；

``Data.Set``
------------

- :hs:mod:`Data.Set`\ 模块提供处理\ :doc:`集合 <set>`\ 的函数；

.. _官网: https://downloads.haskell.org/~ghc/latest/docs/html/libraries/
