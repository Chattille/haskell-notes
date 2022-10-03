.. highlight:: haskell
   :linenothreshold: 10

====
练习
====

.. note::

   所有习题均来自 `Exercism <https://exercism.org/tracks/haskell/exercises/>`_\ 。

简单
====

华氏度和摄氏度
--------------

华氏度\ ``F``\ 和摄氏度\ ``C``\ 的关系为：``C = (F - 32) / 1.8``\ 。

.. rubric:: 题

定义两个函数，华氏度定义为整型，摄氏度为浮点型，实现华氏度和摄氏度的相互转换。转换为摄氏度时，结果向上取整。

.. rubric:: 例

.. code-block:: text

   输入：华氏 -148 度
   输出：摄氏 -100.0 度

.. code-block:: text

   输入：摄氏 0.0 度
   输出：华氏 32 度

.. collapse:: 解答

   .. code-block::

      module Temperature
          ( tempToC
          , tempToF
          ) where

      type Fahrenheit = Integer
      type Celsius = Float

      -- fromInteger 将整型转换为浮点型
      tempToC :: Fahrenheit -> Celsius
      tempToC temp = (fromInteger temp - 32) / 1.8

      -- ceiling 自动将浮点型转换为整型
      tempToF :: Celsius -> Fahrenheit
      tempToF temp = ceiling $ temp * 1.8 + 32

倍数之和
--------

.. rubric:: 题

给定一串数字和一个上限值，计算该串数字所有小于上限值且不重复的倍数之和。

.. rubric:: 例

.. code-block:: text

   输入：[3, 5] 和 20
   输出：3 + 6 + 9 + 12 + 15 + 18 + 5 + 10 = 78

.. collapse:: 解答 1

   .. tabs::

      .. tab:: SumOfMultiples.hs

         .. code-block::

            module SumOfMultiples
                ( sumOfMultiples
                ) where

            import           Data.Set                      ( fromList
                                                           , toList
                                                           )

            -- 去除小于 0 的数字
            -- 遍历数字列表
            -- 计算每个数字在上限范围内倍数的个数
            --    若数字为 0，则个数也为 0
            -- 逐个列出所有倍数
            -- 去重并求和
            sumOfMultiples :: [Integer] -> Integer -> Integer
            sumOfMultiples factors limit = sum . toList $ fromList -- 备注*
                [ multiple
                | x <- filter (>= 0) factors
                , let entries = if x == 0 then [0] else [1 .. (limit - 1) `div` x]
                , y <- entries
                , let multiple = x * y
                ]

            -- 备注：对于大列表，toList . fromList 的速度比 nub 的速度高得多，
            --      但占用空间也更多。详见 GHCi 中的比较

      .. tab:: GHCi

         .. code-block::
            :emphasize-lines: 4, 7

            ghci> :set +s
            ghci> sumOfMultiples' [3, 5] 100000 -- 使用 'nub'
            2333316668
            (17.31 secs, 19,809,520 bytes)
            ghci> sumOfMultiples [3, 5] 100000 -- 使用 'toList' 和 'fromList'
            2333316668
            (0.10 secs, 33,174,568 bytes)

.. collapse:: 解答 2

   .. code-block::

      module SumOfMultiples
          ( sumOfMultiples
          ) where

      import           Data.Set                       ( fromList
                                                      , toList
                                                      )

      -- 去除小于 0 的数字
      -- 从 1 遍历到上限减 1，判断数字是否为该串数字中任意一个数字的倍数
      -- 筛选出这样的数字并求和
      sumOfMultiples' :: [Integer] -> Integer -> Integer
      sumOfMultiples' factors limit =
          sum $ filter divisableBy [1 .. limit - 1]
        where
          -- 判断某数字是否是该串数字的倍数
          divisableBy :: Integer -> Bool
          divisableBy n = any ((== 0) . (n `mod`)) $ filter (>= 0) factors

      -- 备注：相比解答 1，此实现速度稍慢，占用空间更大

完全数
------

:tr:`完全数 (perfect number)`\ ，也叫完美数，是由古希腊数学家\ :tr:`尼科马库斯 (Nicomachus)`\ 提出的整数分类方法。若一个数除本身外的所有因数之和正好等于该数本身，则该数为完全数，和大于本身的为\ :tr:`盈数 (abundant number)`\ ，小于的为\ :tr:`亏数 (deficient number)`\ （所有质数均为亏数）。

.. rubric:: 题

给定一个非负整数，判断是否为完全数、盈数或亏数。

.. rubric:: 例

.. code-block:: text

   输入：6
   输出：完全数
   解释：1 + 2 + 3 = 6

.. code-block:: text

   输入：12
   输出：盈数
   解释：1 + 2 + 3 + 4 + 6 = 16 > 12

.. code-block:: text

   输入：8
   输出：亏数
   解释：1 + 2 + 4 = 7 < 8

.. collapse:: 解答

   .. code-block::

      module PerfectNumbers
          ( classify
          , Classification(..)
          ) where

      data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

      -- 判断数字是否有效
      -- 寻找因数并求和，再根据和判断是否为完全数、盈数或亏数
      classify :: Int -> Maybe Classification
      classify num | num > 0   = Just $ aliquot num
                   | otherwise = Nothing
        where
          -- 根据因数的和判断类型
          aliquot :: Int -> Classification
          aliquot n | sum (factors n) == n = Perfect
                    | sum (factors n) > n  = Abundant
                    | otherwise            = Deficient
          -- 分解因数
          -- 最大因数只能为该数的一半，因此不用遍历至 n - 1
          factors :: Int -> [Int]
          factors n = [ x | x <- [1 .. n `div` 2], n `mod` x == 0 ]

电话号码
--------

:tr:`北美编号方案 (North American Numbering Plan)`\ 是一种在北美地区广泛使用的电话号码编号系统。该方案由一个国家码后跟 10 位数组成，后 10 位又由 3 位区域码和 7 位本地码组成，而本地码又由 3 位交换码和 4 为用户码组成，即：

.. code-block::

   (NXX)-NXX-XXXX

其中\ ``N``\ 取值范围为 2 到 9 的闭区间，\ ``X``\ 的取值范围为 0 到 9 的闭区间。

.. rubric:: 题

给定一串号码，输出有效的 10 位号码。由于本题仅处理北美编号方案的号码，因此国家码只有 1 有效。

.. rubric:: 例

.. code-block:: text

   输入：+1 (613)-995-0253
   输出：6139950253

.. code-block:: text

   输入：613.995.0253
   输出：6139950253

.. collapse:: 解答 1

   .. code-block::

      module Phone
          ( number
          ) where

      import           Data.Char                      ( isNumber )

      -- 提取数字后判断国家码
      -- 去除国家码并验证剩余号码是否有效
      number :: String -> Maybe String
      number [] = Nothing
      number nums
          | length cleaned == 11 && head cleaned == '1'
          = validate . tail $ cleaned
          | length cleaned == 10
          = validate cleaned
          | otherwise
          = Nothing
        where
          -- 提取数字
          cleaned :: String
          cleaned = filter isNumber nums
          -- 若第 1 和第 4 位数字在 2 到 9 之内，则号码有效
          validate :: String -> Maybe String
          validate ns | isValid 0 ns && isValid 3 ns = Just ns
                      | otherwise                    = Nothing
            where
              -- 对应数字必须在 2 到 9 之内
              isValid :: Int -> String -> Bool
              isValid index xs = '2' <= xs !! index && xs !! index <= '9'

.. collapse:: 解答 2

   .. code-block::

      module Phone
          ( number
          ) where

      import           Data.Char                      ( isDigit )

      number :: String -> Maybe String
      number = check . dropCountryCode . filter isDigit
        where
          -- 第一位为 1 的只能是国家码
          dropCountryCode :: String -> String
          dropCountryCode ns = if head ns == '1' then tail ns else ns
          -- 守卫匹配失败的会继续下一个模式匹配
          check :: String -> Maybe String
          check phone@[a, _, _, b, _, _, _, _, _, _]
              | a > '1' && b > '1' = Just phone
          check _ = Nothing

克拉兹猜想
----------

:tr:`克拉兹猜想 (Collatz conjecture)`\ 可描述为如下：

有任意整数 :math:`n`\ ，若该整数为奇数，则将该数乘以 3 后加 1 得到 :math:`n \times 3 + 1`\ ；若为偶数，则除以 2 得到 :math:`n \div 2`\ ；重复该过程。克拉兹猜想认为，无论起始值是多少，最终都会得到 1。

.. rubric:: 题

给定起始值 :math:`n`\ ，返回得到 1 需要的最少步骤。

.. rubric:: 例

.. code-block:: text

   输入：5
   输出：5
   解释：1. 5 * 3 + 1 = 16
        2. 16 / 2    = 8
        3. 8 / 2     = 4
        4. 4 / 2     = 2
        5. 2 / 2     = 1

.. collapse:: 解答 1

   .. code-block::

      module CollatzConjecture
          ( collatz
          ) where

      -- 判断是否为有效数字
      -- 使用无限列表无限计算下去，直到得到数字 1
      -- 从起始值到 1 之间元素的个数即为最少步骤
      collatz :: Integer -> Maybe Integer
      collatz n | n <= 0    = Nothing
                | otherwise = return $ steps n
        where
          -- 打印从起始值开始无限计算下去的所有中间值
          allValues :: Integer -> [Integer]
          allValues n = scanl (flip ($)) n $ repeat nextValue
          -- 计算下一个值
          nextValue :: Integer -> Integer
          nextValue n | odd n       = n * 3 + 1
                      | otherwise n = n `div` 2
          -- 1 之前的中间值个数
          steps :: Integer -> Integer
          steps = toInteger . length . takeWhile (/= 1) . allValues

.. collapse:: 解答 2

   .. code-block::

      module CollatzConjecture
          ( collatz
          ) where

      -- 判断数字是否有效
      -- 数字为 1 时，返回 Just 0，该值为递归的出口
      -- 每返回一层递归，便加 1，递归结束便得到最少步骤
      collatz :: Integer -> Maybe Integer
      collatz n | n <= 0    = Nothing
                | n == 1    = Just 0
                | even n    = succ <$> collatz (n `div` 2)
                | otherwise = succ <$> collatz (n * 3 + 1)

统计碱基
----------

:tr:`核苷酸 (nucleotide)`\ 是 DNA 和 RNA 的组成部分。组成 DNA 的核苷酸中包含 :abbr:`A (adenine)`\ 、\ :abbr:`C (cytosine)`\ 、\ :abbr:`G (guanine)` 和 :abbr:`T (thymine)` 四种\ :tr:`碱基 (nucleobase)`\ 。通常将 DNA 序列表示为一串由“ACGT”四个字母组成的字符串。

.. rubric:: 题

给定一串 DNA 序列，统计每个碱基的数量，无效输入应报错。

.. rubric:: 例

.. code-block:: text

   输入："GATTACA"
   输出：'A': 3, 'C': 1, 'G': 1, 'T': 2

.. code-block:: text

   输入："INVALID"
   输出：报错

.. collapse:: 解答 1

   .. code-block::

      module DNA
          ( nucleotideCounts
          , Nucleotide(..)
          ) where

      import           Data.Map                       ( Map
                                                      , fromList
                                                      )

      data Nucleotide = A | T | G | C deriving (Eq, Ord, Show)

      -- 判断 DNA 是否有效
      -- 使用 filter 筛选指定碱基并统计个数
      -- 统计所有碱基并返回结果
      nucleotideCounts :: String -> Either String (Map Nucleotide Int)
      nucleotideCounts xs
          | all (`elem` "ATGC") xs = Right (countNucleotides xs)
          | otherwise              = Left "Invalid DNA"
        where
          -- 统计指定碱基在 DNA 序列中的个数
          amount :: Char -> String -> Int
          amount ch = length . filter (== ch)
          -- 统计四种碱基在 DNA 序列中的个数并返回映射
          countNucleotides :: String -> Map Nucleotide Int
          countNucleotides dna = fromList
              [ (A, amount 'A' dna)
              , (T, amount 'T' dna)
              , (G, amount 'G' dna)
              , (C, amount 'C' dna)
              ]

.. collapse:: 解答 2

   .. code-block::

      module DNA
          ( nucleotideCounts
          , Nucleotide(..)
          ) where

      import           Data.Char                      ( toUpper )
      import           Data.Map                       ( Map
                                                      , fromListWith
                                                      )

      data Nucleotide = A | T | G | C deriving (Eq, Ord, Show, Read)

      -- 判断 DNA 是否有效
      -- 创建默认映射
      -- 遍历 DNA，遇到相应碱基便加 1
      --    fromListWith 可处理重复的键，对逐字统计很有用
      -- 返回结果
      nucleotideCounts :: String -> Either String (Map Nucleotide Int)
      nucleotideCounts xs
          | all (`elem` "ATGC") xs = Right $ countNucleotides xs
          | otherwise              = Left "Invalid DNA"
        where
          -- 默认数量
          defaultCounts :: [(Nucleotide, Int)]
          defaultCounts = [(A, 0), (T, 0), (G, 0), (C, 0)]
          -- 逐个统计碱基数量，用 'fromListWith' 处理重复的键
          countNucleotides :: String -> Map Nucleotide Int
          countNucleotides dna =
              fromListWith (+)
                  $  [ (read [toUpper d], 1) | d <- dna ]
                  ++ defaultCounts

      -- 备注：相比解答 1，此实现速度更慢，占用空间更大

RNA 转录
--------

RNA 转录是遗传信息由 DNA 转换到 RNA 的过程。DNA 和 RNA 均由一系列核苷酸组成，DNA 包含 :abbr:`A (adenine)`\ 、\ :abbr:`C (cytosine)`\ 、\ :abbr:`G (guanine)` 和 :abbr:`T (thymine)` 四种碱基，而 RNA 包含 :abbr:`A (adenine)`\ 、\ :abbr:`C (cytosine)`\ 、\ :abbr:`G (guanine)` 和 :abbr:`U (uracil)` 四种碱基。

DNA 根据碱基互补原则生成 RNA：

- G 转换为 C；
- C 转换为 G；
- A 转换为 U；
- T 转换为 A；

.. rubric:: 题

给定一串 DNA 序列，返回 RNA 序列，无效输入应返回第一个无效字符。

.. rubric:: 例

.. code-block:: text

   输入："ACGTTA"
   输出："UGCAAU"

.. code-block:: text

   输入："AADTTE"
   输出：'D'

.. collapse:: 解答 1

   .. code-block::

      module DNA
          ( toRNA
          ) where

      -- 转录单个碱基
      -- 合并两个碱基，使用 <$> 和 <*> 合并两个函子
      -- 通过 foldl 合并多个转录后的碱基
      toRNA :: String -> Either Char String
      toRNA = foldl mergeNuc (pure "") . map transcribe
        where
          -- 合并两个转录后的碱基
          mergeNuc :: Either Char String -> Either Char String
              -> Either Char String
          mergeNuc x y = (++) <$> x <*> y
          -- 转录单个碱基
          transcribe :: Char -> Either Char String
          transcribe dna | dna == 'A' = Right "U"
                         | dna == 'T' = Right "A"
                         | dna == 'G' = Right "C"
                         | dna == 'C' = Right "G"
                         | otherwise  = Left dna

.. collapse:: 解答 2

   .. code-block::

      module DNA
          ( toRNA
          ) where

      type Nucleotide = Char
      type DNA = [Nucleotide]
      type RNA = [Nucleotide]

      -- 转录单个碱基
      -- 合并多个碱基，使用 mapM 直接将函子列表合并为一个包含列表的函子
      toRNA :: DNA -> Either Nucleotide RNA
      toRNA = mapM transcribe
      -- mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
        where
          -- 转录单个碱基
          transcribe :: Nucleotide -> Either Nucleotide Nucleotide
          transcribe nuc | nuc == 'A' = Right 'U'
                         | nuc == 'T' = Right 'A'
                         | nuc == 'G' = Right 'C'
                         | nuc == 'C' = Right 'G'
                         | otherwise  = Left nuc

秘密通信
--------

.. rubric:: 题

假设有一种二进制秘密通信叫“handshake”，其规则如下：

.. code-block:: text

   1 = wink
   10 = double blink
   100 = close your eyes
   1000 = jump
   10000 = 将操作倒序排序

给定一个整数，按上述规则返回秘密通信的操作序列。

.. rubric:: 例

.. code-block:: text

   输入：3
   输出：wink 和 double wink
   解释：3 的二进制为 11，即 10 + 1

.. code-block:: text

   输入：19
   输出：double wink 和 wink

.. collapse:: 解答 1

   .. code-block::

      module SecretHandshake
          ( handshake
          ) where

      -- 将数字转换为二进制
      --    若数字大于 10000，则倒序排序
      -- 将二进制转换为字符串
      -- 将字符串倒序排序，对应索引若为 1，则返回相应索引位置的操作
      handshake :: Int -> [String]
      handshake n = if toBinary n >= 10000
          then reverse . generate . take 4 $ bin
          else generate bin
        where
          -- 二进制转换为字符串后倒序排序
          bin :: String
          bin = reverse . show . toBinary $ n
          -- 对应索引为 1，则包括对应索引的操作
          generate :: String -> [String]
          generate s = [ dict !! x | x <- [0 .. length s - 1], s !! x == '1' ]
            where
              dict :: [String]
              dict = ["wink", "double blink", "close your eyes", "jump"]
          -- 转换为（伪）二进制
          toBinary :: Int -> Int
          toBinary 0 = 0
          toBinary d = 10 * toBinary (d `div` 2) + d `mod` 2

.. collapse:: 解答 2

   .. code-block::

      module SecretHandshake
          ( handshake
          ) where

      handshake :: Int -> [String]
      handshake = handshakeAcc []
        where
          -- 从大到小逐个减去，不必转换为二进制
          handshakeAcc :: [String] -> Int -> [String]
          handshakeAcc acc i
              | i >= 16   = reverse $ handshakeAcc acc (i - 16)
              | i >= 8    = handshakeAcc ("jump" : acc) (i - 8)
              | i >= 4    = handshakeAcc ("close your eyes" : acc) (i - 4)
              | i >= 2    = handshakeAcc ("double blink" : acc) (i - 2)
              | i == 1    = handshakeAcc ("wink" : acc) (i - 1)
              | otherwise = acc

首字母缩写
----------

众所周知，程序员喜欢 :abbr:`TLA (three letter acronym)`\ （三字母缩写）。

.. rubric:: 题

给定一个字符串，生成首字母缩写词。

.. rubric:: 例

.. code-block:: text

   输入："Portable Network Graphics"
   输出："PNG"

.. code-block:: text

   输入："Ruby on Rails"
   输出："ROR"

.. code-block:: text

   输入："HyperText Markup Language"
   输出："HTML"
   解释：驼峰命名法

.. code-block:: text

   输入："The Road _Not_ Taken"
   输出："TRNT"
   解释：强调

.. code-block:: text

   输入："Complementary metal-oxide semiconductor"
   输出："CMOS"
   解释：连字符

.. code-block:: text

   输入："Halley's Comet"
   输出："HC"

.. collapse:: 解答 1

   .. code-block::

      module Acronym
          ( abbreviate
          ) where

      import           Data.Char                      ( isAlpha
                                                      , isUpper
                                                      )
      import qualified Data.Text                     as T

      -- 替换连字符为空格，方便分词
      -- 分词
      -- 去除单词首尾的非字母字符
      -- 首字母未大写的单词转换为大写
      -- 将小写字母替换为 '-'，方便处理驼峰大写
      -- 按 '-' 拆分单词
      -- 取首字母并合并
      abbreviate :: String -> String
      abbreviate = map T.head . splitText . T.pack

      -- 将字符串转换为多组大写字母的列表
      splitText :: T.Text -> [T.Text]
      splitText =
          concatMap (splitCamels . toCapital . noPunctuations)
              . T.words
              . noHyphens
        where
          -- 将连字符转换为空格以便分词
          noHyphens :: T.Text -> T.Text
          noHyphens = T.replace (T.pack "-") (T.pack " ")
          -- 将小写字母替换为 '-' 以便区分驼峰命名和全大写
          --    HyperText 变为 H----T----
          --    GNU       变为 GNU
          splitCamels :: T.Text -> [T.Text]
          splitCamels = filter (not . T.null)
              . T.splitOn (T.singleton '-')
              . T.map (\c -> if isUpper c then c else '-')
          -- 去除首尾非字母字符
          noPunctuations :: T.Text -> T.Text
          noPunctuations = T.dropAround (not . isAlpha)
          -- 首字母大写
          toCapital :: T.Text -> T.Text
          toCapital t = let (l, r) = T.splitAt 1 t in T.toUpper l <> r

.. collapse:: 解答 2

   .. code-block::

      module Acronym
          ( abbreviate
          ) where

      import           Data.Char                      ( isAlpha
                                                      , isLower
                                                      , isUpper
                                                      , toUpper
                                                      )

      -- 自左向右逐个处理字符
      -- 用二元元组保存上一字符和提取的有效首字母
      --    若上一字符为非字母但不为 "'"，当前字符为字母，保留
      --    若上一字符为小写，当前字符为大写，保留（驼峰）
      --    其余情况，跳过
      -- 返回提取的有效首字母
      abbreviate :: String -> String
      abbreviate = snd . foldl keepChar (' ', [])
        where
          -- 比较前后两个字符并保留有效首字母
          keepChar :: (Char, String) -> Char -> (Char, String)
          keepChar (prev, abbr) crt
              | (not (isAlpha prev) && prev /= '\'') && isAlpha crt
              = (crt, abbr ++ [toUpper crt])
              | isLower prev && isUpper crt
              = (crt, abbr ++ [crt])
              | otherwise
              = (crt, abbr)

普通
====

异序词
------

:tr:`同素异序词 (anagram)`\ ，指所用字母相同但字母顺序不同而组成的新单词。

.. rubric:: 题

给定一个词和一组词，从该组词中找出该词的同素异序词。

.. rubric:: 例

.. code-block:: text

   输入："listen" 和 ["enlists", "google", "inlets", "banana"]
   输出：["inlets"]

.. code-block:: text

   输入："master" 和 ["Stream", "pigeon", "maters"]
   输出：["Stream", "maters"]
   解释：大小写不敏感

.. code-block:: text

   输入："go" 和 ["go", "GO"]
   输出：[]
   解释：同素异序词不能为单词本身

.. collapse:: 解答 1

   .. code-block::

      module Anagram
          ( anagramsFor
          ) where

      import           Data.Char                      ( toLower )
      import           Data.Map                       ( Map
                                                      , fromListWith
                                                      )

      -- 将字符串转换为小写
      -- 统计单词中字母的出现次数
      --    若字频相同，且两个字符串不同，则为异序词
      anagramsFor :: String -> [String] -> [String]
      anagramsFor pat cands =
          [ cand
          | cand <- cands
          , let c = map toLower cand
          , let p = map toLower pat
          , p /= c
          , frequency p == frequency c
          ]
        where
          -- 统计字母在单词中的出现次数
          frequency :: String -> Map Char Int
          frequency str = fromListWith (+) [ (ch, 1) | ch <- str ]

.. collapse:: 解答 2

   .. code-block::

      module Anagram
          ( anagramsFor
          ) where

      import           Data.Char                      ( toLower )
      import           Data.List                      ( sort )

      -- 将字符串转换为小写
      -- 若排序后两者相同，且原字符串不同，则为异序词
      anagramsFor :: String -> [String] -> [String]
      anagramsFor pat cands =
          [ cand
          | let p = map toLower pat
          , cand <- cands
          , let c = map toLower cand
          , p /= c
          , sort p == sort c
          ]

时钟
----

.. rubric:: 题

实现一个 24 小时制的时钟数据类型，要求该时钟数据为\ ``Eq``\ 类型类的成员，且实现了以下方法：

- :hs:func:`toString`\ ：将时钟数据转换为字符串，0 补位；
- :hs:func:`fromHourMin`\ ：从小时和分钟构造时钟数据；
- :hs:func:`addDelta`\ ：时钟经过指定小时数和分钟数；

.. rubric:: 例

.. code-block:: text

   toString

       输入：8 时 0 分
       输出：08 : 00

       输入：13 时 25 分
       输出：13 : 25

.. code-block:: text

   fromHourMin

       输入：25, 0
       输出：01 : 00

       输入：25, 160
       输出：03 : 40

.. code-block:: text

   addDelta

       输入：0, 65 和 22 : 55
       输出：00 : 00

       输入：0, -30 和 10 : 03
       输出：09 : 33

.. collapse:: 解答 1

   .. code-block::

      module Clock
          ( addDelta
          , fromHourMin
          , toString
          ) where

      type Hour = Int
      type Minute = Int

      data Clock = Clock Hour Minute deriving Eq

      -- 直接相加
      addDelta :: Hour -> Minute -> Clock -> Clock
      addDelta hour minute (Clock h m) = fromHourMin (hour + h) (minute + m)

      -- 对分钟数取模
      -- 时钟数加上分钟数超过的部分再取模
      fromHourMin :: Hour -> Minute -> Clock
      fromHourMin hour minute =
          let m = minute `mod` 60
              h = (hour + minute `div` 60) `mod` 24
          in  Clock h m

      -- 将数字字符串化
      -- 判断字符串长度
      --    若为 1，添加 0
      --    否则，保留原样
      toString :: Clock -> String
      toString (Clock hour minute) = padding hour ++ ":" ++ padding minute
        where
          padding :: Int -> String
          padding n | length (show n) == 1 = '0' : show n
                    | otherwise            = show n

.. collapse:: 解答 2

   .. code-block::

      module Clock
          ( addDelta
          , fromHourMin
          , toString
          ) where

      import           Text.Printf                    ( printf )

      -- 以分钟数储存时间
      newtype Clock = Clock {getMinutes :: Int} deriving Eq

      -- 一天总分钟数为 1440 分钟
      fromHourMin :: Int -> Int -> Clock
      fromHourMin h m = Clock $ (h * 60 + m) `mod` 1440

      -- printf 函数更简单
      -- divMod 函数方便地将商、余打包，实现进位效果
      toString :: Clock -> String
      toString clock =
          let (h, m) = getMinutes clock `divMod` 60 in printf "%02d:%02d" h m

      addDelta :: Int -> Int -> Clock -> Clock
      addDelta h m clock = fromHourMin h $ getMinutes clock + m

二叉查找树
----------

:tr:`二叉查找树 (binary search tree)`\ 是一种有序数据结构。

假设有顺序数组\ ``[1, 3, 4, 5]``\ ，当向该数组插入\ ``2``\ 时，有两种插入方案：

1. 追加到数组末尾并对整个数组重新排序；
2. 找到\ ``2``\ 的正确位置后添加空位并插入；

两种插入方案都会消耗大量的空间或时间。为了高效处理有序数据，二叉查找树应运而生。

二叉树由一系列\ :tr:`节点 (node)`\ 组成。每个节点都包含一个数据，左子树和右子树，\ :tr:`子树 (subtree)`\ 又指向另一个子树或空节点。子树中，左子树包含所有小于或等于当前节点的值，右子树包含所有大于当前节点的值。例如，当向以下二叉树插入\ ``6``\ 时：

.. code-block:: text

     4
    /
   2

结果为：

.. code-block:: text

     4
    / \
   2   6

再次插入\ ``3``\ 后结果为：

.. code-block:: text

      4
    /   \
   2     6
    \
     3

.. rubric:: 题

定义一个二叉树数据类型\ ``BST``\ 并使其成为\ ``Eq``\ 和\ ``Show``\ 类型类的成员。要求实现下列函数：

- :hs:func:`bstLeft`\ ：返回左子树；
- :hs:func:`bstRight`\ ：返回右子树；
- :hs:func:`bstValue`\ ：返回当前节点的值；
- :hs:func:`empty`\ ：返回空节点；
- :hs:func:`fromList`\ ：根据列表生成二叉树；
- :hs:func:`insert`\ ：将值插入二叉树中；
- :hs:func:`singleton`\ ：返回仅包含一个节点的二叉树；
- :hs:func:`toList`\ ：根据二叉树生成列表；

.. collapse:: 解答

   .. code-block::

      module BST
          ( BST
          , bstLeft
          , bstRight
          , bstValue
          , empty
          , fromList
          , insert
          , singleton
          , toList
          ) where

      -- 递归定义代数数据类型
      data BST a = EmptyNode | Node a (BST a) (BST a) deriving (Eq, Show)

      bstLeft :: BST a -> Maybe (BST a)
      bstLeft EmptyNode    = Nothing
      bstLeft (Node _ l _) = Just l

      bstRight :: BST a -> Maybe (BST a)
      bstRight EmptyNode    = Nothing
      bstRight (Node _ _ r) = Just r

      bstValue :: BST a -> Maybe a
      bstValue EmptyNode    = Nothing
      bstValue (Node x _ _) = Just x

      empty :: BST a
      empty = EmptyNode

      -- 遍历列表并逐个将每个值插入空节点中
      fromList :: Ord a => [a] -> BST a
      fromList = foldl (flip insert) EmptyNode

      insert :: Ord a => a -> BST a -> BST a
      insert x EmptyNode = singleton x
      insert x (Node a l r) | x > a     = Node a l (insert x r)
                            | otherwise = Node a (insert x l) r

      singleton :: a -> BST a
      singleton x = Node x EmptyNode EmptyNode

      toList :: BST a -> [a]
      toList EmptyNode    = []
      toList (Node x l r) = toList l ++ [x] ++ toList r

皇后攻击
--------

国际象棋中，皇后可以攻击同一行、同一列和同一对角线上的棋子。

.. rubric:: 题

实现以下函数：

- :hs:func:`boardString`\ ：打印 :math:`8 \times 8` 大小的棋盘，白皇后用\ ``W``\ 表示，黑皇后用\ ``B``\ 表示，空白格子用\ ``_``\ 表示：

  .. code-block:: text

     _ _ _ _ _ _ _ _
     _ _ _ _ _ _ _ _
     _ _ _ W _ _ _ _
     _ _ _ _ _ _ _ _
     _ _ _ _ _ _ _ _
     _ _ _ _ _ _ B _
     _ _ _ _ _ _ _ _
     _ _ _ _ _ _ _ _

- :hs:func:`canAttack`\ ：给定两个皇后的坐标\ ``(row, column)``\ ，判断两个皇后是否能相互攻击；

.. rubric:: 例

.. code-block:: text

   输入：(2, 3) 和 (5, 6)
   输出：boardString 返回题目中的棋盘，canAttack 返回 True

.. collapse:: 解答

   .. code-block::

      module Queens
          ( boardString
          , canAttack
          ) where

      type Board = String
      type Coordinates = (Int, Int)
      type Queen = Maybe Coordinates

      -- 逐个判断棋子
      boardString :: Queen -> Queen -> Board
      boardString white black = unlines
          [ unwords
                [ chess
                | c <- [0 .. 7]
                , let chess | Just (r, c) == white = "W"
                            | Just (r, c) == black = "B"
                            | otherwise            = "_"
                ]
          | r <- [0 .. 7]
          ]

      -- 同一行行号相等，同一列列号相等
      -- 同一对角线横、纵坐标差值的绝对值相等
      canAttack :: Coordinates -> Coordinates -> Bool
      canAttack queenA queenB =
          rA == rB || cA == cB || abs (rA - rB) == abs (cA - cB)
        where
          (rA, cA) = queenA
          (rB, cB) = queenB

杨辉三角
--------

杨辉三角，也称\ :tr:`帕斯卡三角 (Pascal's triangle)`\ ，每一个数都由上一行左右两侧的数相加而来。

.. rubric:: 题

给定一个整数行号，返回从第一行到该行部分的杨辉三角。

.. rubric:: 例

.. code-block:: text

   输入：4
   输出：1
       1 1
      1 2 1
     1 3 3 1

.. code-block:: text

   输入：0
   输出：无

.. collapse:: 解答 1

   .. code-block::

      module Triangle
          ( rows
          ) where

      -- 根据上一行计算当前行
      -- 添加缺失的首尾
      rows :: Int -> [[Integer]]
      rows x | x <= 0    = []
             | x == 1    = [[1]]
             | otherwise = prevRows ++ [[1] ++ walk (last prevRows) ++ [1]]
        where
          -- 当前行之前的所有行
          prevRows :: [[Integer]]
          prevRows = rows $ x - 1
          -- 将上一行两两相加
          walk :: [Integer] -> [Integer]
          walk l@(a : b : _) = a + b : walk (tail l)
          walk _             = []

.. collapse:: 解答 2

   .. code-block::

      module Triangle
          ( rows
          ) where

      -- 不断对前一结果应用相同函数、并保留所有结果的操作
      -- 适合使用函数 iterate
      rows :: Int -> [[Integer]]
      rows x = take x $ iterate next [1]
        where
          -- 首尾添加 0 后错位相加
          next :: [Integer] -> [Integer]
          next row = zipWith (+) (0 : row) (row ++ [0])

栅栏加密法
----------

:tr:`栅栏加密法 (crypto square)`\ 是一种古典加密法。

对于明文，仅保留明文中的字母和数字，然后将明文从左到右、从上到下书写为\ ``c``\ 列\ ``r``\ 行的方块，按列从上到下、从左到右重新将文本书写为一行。将文本分成\ ``c``\ 个\ ``r``\ 长度的文本块，并用空格分隔，若文本长度比\ ``c * r``\ 短\ ``n``\ 个字符，则在最后\ ``n``\ 个文本块末尾各补一个空格，得到最后的密文。

.. rubric:: 题

给定一个明文字符串，输出加密后的密文。


.. rubric:: 例

.. code-block:: text

   输入："Hello World!"
   输出："hol ewd lo  lr "
   解释：1. "helloworld"
        2. "hell"
           "owor"
           "ld  "
        3. "holewdlolr"
        4. "hol ewd lo  lr "

.. collapse:: 解答

   .. code-block::

      module CryptoSquare
          ( encode
          ) where

      import           Data.Char                      ( isAlphaNum
                                                      , toLower
                                                      )
      import           Data.List                      ( transpose )

      -- 转置后直接用空格连接文本块便可得到要求的密文
      encode :: String -> String
      encode xs =
          let ns     = map toLower . filter isAlphaNum $ xs
              l      = length ns
              (c, r) = square l
              chunks = chunksOf c $ ns ++ replicate (c * r - l) ' '
          in  unwords . transpose $ chunks
        where
          -- 递归在指定索引处分隔字符串
          chunksOf :: Int -> String -> [String]
          chunksOf _ []  = []
          -- 也可以使用 take n str : chunksOf n (drop n str)
          chunksOf n str = let (f, s) = splitAt n str in f : chunksOf n s
          -- 平方根后向上取整，得到最接近正方形边长的值
          square :: Int -> (Int, Int)
          square n =
              let c = ceiling $ sqrt (fromIntegral n :: Double)
              in  if c * (c - 1) >= n then (c, c - 1) else (c, c)

Luhn 算法
---------

Luhn 算法，一种简单的\ :tr:`校验和 (checksum)`\ 算法。Luhn 算法通常用于身份识别码，如国际移动设备辨识码、加拿大社会保险码等。Luhn 算法的校验步骤如下：

1. 小于两位数的无效；
2. 去除空格；
3. 从右向左，奇数位不变，偶数位乘 2，若乘 2 后结果为两位数，则结果减去 9；
4. 将所有数字相加；
5. 若和能被 10 整除，则该数字为有效数字，否则无效；

.. rubric:: 题

给定一串数字字符串，使用 Luhn 算法判断是否有效。

.. rubric:: 例

.. code-block:: text

   输入："4539 3195 0343 6467"
   输出：有效

.. code-block:: text

   输入："8273 1232 7352 0569"
   输出：无效

.. code-block:: text

   输入："0"
   输出：无效

.. collapse:: 解答 1

   .. code-block::

      module Luhn
          ( isValid
          ) where

      import           Data.Char                      ( digitToInt
                                                      , intToDigit
                                                      , isDigit
                                                      )

      -- 倒序排序
      -- zipWith 为每个字符标号
      -- 根据奇偶位进行计算
      -- 验证总和
      isValid :: String -> Bool
      isValid ds
          | length fn <= 1
          = False
          | otherwise
          = checksum
              . map snd
              . zipWith (curry validate) [1 .. length fn]
              -- 倒序排序后再编号
              $ reverse fn
        where
          -- 去除空格
          fn :: String
          fn = filter isDigit ds
          -- 将每个字符进行编号后判断奇偶位
          validate :: (Int, Char) -> (Int, Char)
          validate (n, d) | odd n     = (n, d)
                          | otherwise = (n, intToDigit doubled)
            where
              doubled =
                  let db = digitToInt d * 2
                  in  if db >= 10 then db - 9 else db
          -- 求和后取模
          checksum :: String -> Bool
          checksum xs = sum (map digitToInt xs) `mod` 10 == 0

.. collapse:: 解答 2

   .. code-block::

      module Luhn
          ( isValid
          ) where

      import           Data.Char                      ( digitToInt
                                                      , isDigit
                                                      )

      isValid :: String -> Bool
      isValid ds | length (normalize ds) < 2 = False
                 | otherwise = luhn (normalize ds) `mod` 10 == 0
        where
          -- 转化为数字后直接校验，不必转回字符串
          normalize :: String -> [Int]
          normalize = map digitToInt . reverse . filter isDigit
          -- 递归求和，偶数位用 (_ : y : _) 模式匹配
          luhn :: [Int] -> Int
          luhn []  = 0
          luhn [x] = x
          luhn (x : y : zs) =
              x + (if y < 5 then y * 2 else y * 2 - 9) + luhn zs

找质数
------

.. rubric:: 题

给定一个整数\ ``n``\ ，找到第\ ``n``\ 个质数。

.. rubric:: 例

.. code-block:: text

   输入：3
   输出：5

.. code-block:: text

   输入：10001
   输出：104743

.. collapse:: 解答 1

   .. code-block::

      module Prime
          ( nth
          ) where

      -- 在无限列表中筛选出质数，再索引对应值
      nth :: Int -> Maybe Integer
      nth n | n > 0     = Just (toInteger $ filter isPrime [1 ..] !! (n - 1))
            | otherwise = Nothing
        where
          -- 分解质因数，若只有 1 和该数本身，则该数为质数
          isPrime :: Int -> Bool
          isPrime a =
              [ (x, a `div` x) | x <- [1 .. a], a `mod` x == 0 ]
              == [(1, a), (a, 1)]

.. collapse:: 解答 2

   .. code-block::

      module Prime
          ( nth
          ) where

      nth :: Int -> Maybe Integer
      nth n | n > 0     = Just $ primes !! (n - 1)
            | otherwise = Nothing
        where
          primes :: [Integer]
          primes = sieve [2 ..]
          -- 质数不是所有质数的倍数
          sieve :: [Integer] -> [Integer]
          sieve []       = []
          sieve (p : ps) = p : sieve [ x | x <- ps, x `mod` p /= 0 ]

      -- 备注：该实现比解答 1 的速度快得多

扫雷
----

:tr:`扫雷 (minesweeper)`\ 是一款风靡全球的经典游戏。在一句游戏中，雷场上会标记多个数字提示，每个数字代表在以该数字为中心的九宫格中埋下的地雷数，而游戏要求玩家根据数字提示挖出所有地雷。

.. rubric:: 题

若用\ ``*``\ 表示地雷，用空格表示无地雷，给定一个雷场，要求标注出数字提示。

.. rubric:: 例

.. code-block:: text

   输入：·*·*·
        ··*··
        ··*··
   输出：1*3*1
        13*31
        ·2*2·
   解释：为方便表示，此例用 '·' 标注空格
        相邻格子无地雷的，保留空格

.. collapse:: 解答 1

   .. code-block::

      module Minesweeper
          ( annotate
          ) where

      import           Data.Char                      ( intToDigit )

      type Board = [String]
      type Coordinates = (Int, Int)
      type Square = (Coordinates, Char)

      -- 为所有格子标注坐标
      -- 获取指定格子相邻格子的合法坐标，并计算地雷数
      --    若当前格子为地雷，则保留
      --    若有地雷，则替换为地雷数，否则保留
      -- 返回标注结果
      annotate :: Board -> Board
      annotate board = map (map reveal) . enumerate $ board
        where
          -- 雷场的列数
          columns :: Int
          columns = length . head $ board
          -- 雷场的行数
          rows :: Int
          rows = length board
          -- 若该格子为地雷，则保留
          -- 若相邻格子有地雷，则替换为数字，否则保留
          reveal :: Square -> Char
          reveal (coord, c)
              | c == '*'
              = c
              | otherwise
              = let cnt = countMines coord
                in  if cnt > 0 then intToDigit cnt else c
          -- 获取该坐标相邻格子的合法坐标
          -- 若相邻格子有地雷，则返回地雷数
          countMines :: Coordinates -> Int
          countMines (x, y) = length
              [ (x', y')
              | x' <- [x - 1 .. x + 1]
              , x' >= 0 && x' < columns
              , y' <- [y - 1 .. y + 1]
              , y' >= 0 && y' < rows
              , board !! y' !! x' == '*'
              ]
          -- 为雷场的所有格子标注坐标
          enumerate :: Board -> [[Square]]
          enumerate = zipWith
              zip
              [ [ (x, y) | x <- [0 .. columns - 1] ] | y <- [0 .. rows - 1] ]

.. collapse:: 解答 2

   .. code-block::

      module Minesweeper
          ( annotate
          ) where

      import           Data.Char                      ( intToDigit )

      annotate :: [String] -> [String]
      annotate m = zipWith (zipWith toChar) mines adj
        where
          -- 布尔值表示该格是否原为地雷
          mines :: [[Bool]]
          mines = (map . map) (== '*') m
          -- 数字表示地雷数量
          adj :: [[Int]]
          adj = smooth . (map . map $ fromEnum) $ mines
          -- 根据布尔值和数字转换为字符
          toChar :: Bool -> Int -> Char
          toChar True  _ = '*'
          toChar False 0 = ' '
          toChar False n = intToDigit n

      -- 相邻三行相加，然后相邻三列相加
      smooth :: [[Int]] -> [[Int]]
      smooth = map (trips add3 0) . trips (zipWith3 add3) (repeat 0)
        where
          add3 :: Num a => a -> a -> a -> a
          add3 a b c = a + b + c

      -- 相邻三行或三列相加
      -- 在首尾补 0，防止超出索引范围
      trips :: (a -> a -> a -> b) -> a -> [a] -> [b]
      trips f border = go . (++ [border]) . (border :)
        where
          go l@(a : b : c : _) = f a b c : go (tail l)
          go _                 = []
