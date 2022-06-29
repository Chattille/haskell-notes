.. highlight:: haskell
   :linenothreshold: 10

====
集合
====

简介
====

- :tr:`集合 (set)`\ ：一个包含不重复元素的序列；
- 与映射类似，集合也是按树状实现的，因此是\ **有序**\ 的；
- :hs:mod:`Data.Set`\ 模块实现了与集合相关的功能；

定义
====

.. hs:module:: Data.Set

.. hs:function:: fromList :: Ord a => [a] -> Set a

   与\ :hs:func:`Data.Map.fromList`\ 函数类似，将列表转换为集合。

   .. code-block::

      import qualified Data.Set as S

      text1 = "The abyss looks into you."
      text2 = "I think, therefore I am."
      set1 = S.fromList text1 -- fromList " .Tabehiklnostuy"
      set2 = S.fromList text2 -- fromList " ,.Iaefhikmnort"

.. hs:function:: empty :: Set a

   空集合。

   .. code-block::

      exp1 = S.empty -- fromList []

.. hs:function:: singleton :: a -> Set a

   返回一个包含单个元素的集合。

   .. code-block::

      exp2 = S.singleton 3 -- fromList [3]

添加
====

.. hs:function:: insert :: Ord a => a -> Set a -> Set a

   将元素插入集合最后。

   .. code-block::

      exp3 = S.insert 10 $ S.fromList [1 .. 3] -- fromList [1,2,3,10]

删除
====

.. hs:function:: delete :: Ord a => a -> Set a -> Set a

   从集合删除所有匹配元素。

   .. code-block::

      exp4 = S.delete 4 $ S.fromList [3, 4, 5, 4, 3] -- fromList [3,5]

.. hs:function:: deleteAt :: Int -> Set a -> Set a

   从集合删除指定索引对应值。

   .. code-block::

      exp5 = S.deleteAt 2 $ S.fromList [1 .. 5] -- fromList [1,2,4,5]

访问
====

.. hs:function:: take :: Int -> Set a -> Set a

   从集合中取出相应数量的元素并返回新集合。

   .. code-block::

      exp6 = S.take 3 $ S.fromList [1 .. 5] -- fromList [1,2,3]

.. hs:function:: drop :: Int -> Set a -> Set a

   从集合中移除相应数量的元素并返回剩余元素。

   .. code-block::

      exp7 = S.drop 3 $ S.fromList [1 .. 5] -- fromList [4,5]

统计
====

.. hs:function:: size :: Set a -> Int

   统计集合所包含元素的个数。

判断
====

.. hs:function:: null :: Set a -> Bool

   判断集合是否为空。

.. hs:function:: member :: Ord a => a -> Set a -> Bool

   判断元素是否属于集合。

.. hs:function:: isSubsetOf :: Ord a => Set a -> Set a -> Bool

   判断第一个集合是否为第二个集合的子集。

   .. code-block::

      exp8 = S.fromList [2] `S.isSubsetOf` S.fromList [2, 3] -- True
      exp9 = S.fromList [2] `S.isSubsetOf` S.fromList [2]    -- True

.. hs:function:: isProperSubsetOf :: Ord a => Set a -> Set a -> Bool

   判断第一个集合是否为第二个集合的真子集。

   .. code-block::

      exp10 = S.fromList [2] `isProperSubsetOf` S.fromList [2, 3]
              -- True
      exp11 = S.fromList [2] `S.isProperSubsetOf` S.fromList [2]
              --False

运算
====

.. hs:function:: intersection :: Ord a => Set a -> Set a -> Set a

   对两个集合进行交集运算。

   .. code-block::

      exp12 = S.intersection set1 set2 -- fromList " .aehiknot"

.. hs:function:: difference :: Ord a => Set a -> Set a -> Set a

   对第一个集合进行差集运算。

   .. code-block::

      exp13 = S.difference set1 set2 -- fromList "Tblsuy"

.. hs:function:: union :: Ord a => Set a -> Set a -> Set a

   对两个集合进行并集运算。

   .. code-block::

      exp14 = S.union set1 set2 -- fromList " ,.ITabefhiklmnorstuy"

重构
====

.. hs:function:: map :: Ord b => (a -> b) -> Set a -> Set b

   对每个元素进行相同操作。

   .. code-block::

      exp15 = S.map (+ 1) $ S.fromList [3, 2, 5, 3] -- fromList [3,4,6]

.. hs:function:: filter :: (a -> Bool) -> Set a -> Set a

   对集合元素进行筛选。

   .. code-block::

      exp16 = S.filter odd $ S.fromList [3, 4, 5, 3, 4] -- fromList [3,5]

.. hs:function:: toList :: Set a -> [a]

   将集合转换为列表。

   .. tip::

      使用\ :hs:func:`fromList`\ 和\ :hs:func:`toList`\ 函数对大型列表去重的效率比\ :hs:func:`Data.List.nub`\ 函数\ :ref:`高得多 <exercise:倍数之和>`\ 。

   .. note::

      \ :hs:func:`Data.List.nub`\ 函数只要求元素类型为\ ``Eq``\ 成员，而\ :hs:func:`fromList`\ 和\ :hs:func:`toList`\ 函数要求元素类型为\ ``Ord``\ 成员。

      \ :hs:func:`Data.List.nub`\ 函数不会打乱原列表顺序，而\ :hs:func:`fromList`\ 和\ :hs:func:`toList`\ 函数则会。

   .. code-block::

      exp17 = S.toList $ S.fromList [3, 4, 3, 2, 6, 5, 6] -- [2,3,4,5,6]
