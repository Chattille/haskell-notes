.. highlight:: haskell
   :linenothreshold: 10

====
映射
====

简介
====

- :tr:`关联列表 (association list)`\ ：即一个键值相对应的列表，类似于其他语言中的字典或哈希表；
- 在 Haskell 中，原始的关联列表以序对列表的形式呈现；

  .. code-block::

     [(key, value), ...]

  - 一个序对即一个键值对；

    .. code-block::

       exp1 = [("Math", 82), ("English", 94), ("History", 83)]

  - 这种序对列表不保证键的唯一性；

    .. code-block::

       exp2 = [("Math", 82), ("Math", 84)]

  - :hs:mod:`Data.List`\ 的\ :hs:func:`lookup`\ 函数能在序对列表中根据序对的第一个值（键）查询第二个值（值）；

    .. code-block::

       -- lookup :: Eq a => a -> [(a, b)] -> Maybe b
       exp3 = lookup 3 [(3, 4), (1, 2)] -- Just 4

- :hs:mod:`Data.Map`\ 模块实现了严格关联列表，称为\ :tr:`映射 (map)`\ ，数据类型为\ ``Map k a``\ ；
- 当键为\ ``Ord``\ 类型类成员时，应当始终使用\ :hs:mod:`Data.Map`\ 模块来处理键值对数据；

定义
====

.. hs:module:: Data.Map

.. hs:function:: fromList :: Ord k => [(k, a)] -> Map k a

   接受一个序对列表，将其转换为映射，转换时\ **忽略重复的键**\ 。

   根据\ :hs:func:`fromList`\ 函数的签名，序对中的键必须为\ ``Ord``\ **类型类的成员**\ ，因为\ :hs:func:`fromList`\ 函数按树状排列键值对（有序）。

   .. code-block::

      import qualified Data.Map as M

      exp1 = M.fromList [(1, 2), (3, 4), (3, 2), (5, 5)]
      -- fromList [(1,2),(3,4),(5,5)]

.. hs:function:: fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> Map k a

   与\ :hs:func:`fromList`\ 函数类似，但接受一个函数，当两个键重复时，该函数负责处理重复的键对应的两个值。

   .. code-block::

      phoneBook :: [(String, String)]
      phoneBook = 
          [ ("betty", "555-2938")
          , ("betty", "342-2492")
          , ("bonnie", "452-2928")
          ]

      exp2 = M.fromListWith (\n1 n2 -> n1 ++ ", " ++ n2) phoneBook
      -- fromList [("betty","342-2492, 555-2938"),("bonnie","452-2928")]

.. hs:function:: empty :: Map k a

   空映射。

   .. code-block::

      exp3 = M.empty -- fromList []

.. hs:function:: singleton :: k -> a -> Map k a

   接受一个键和一个值，返回包含该键值对的映射。

   .. code-block::

      exp4 = M.singleton 3 9 -- fromList [(3,9)]

访问
====

.. hs:function:: lookup :: Ord k => k -> Map k a -> Maybe a

   与\ :hs:func:`Data.List.lookup`\ 函数类似，但作用于映射。

   .. code-block::

      exp5 = M.lookup 3 $ M.fromList [(2, 5), (3, 4)] -- Just 4

.. hs:function:: keys :: Map k a -> [k]

   以列表方式返回映射的所有键。

   .. code-block::

      exp6 = M.keys $ M.fromList [(1, 2), (3, 4), (5, 6)] -- [1,3,5]

.. hs:function:: elems :: Map k a -> [a]

   以列表方式返回映射的所有值。

添加
====

.. hs:function:: insert :: Ord k => k -> a -> Map k a -> Map k a

   接受一个键、一个值和一个映射，将键值对插入到映射结尾并返回一个新映射。

   .. code-block::

      exp7 = M.insert 3 100 M.empty -- fromList [(3,100)]

.. hs:function:: insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a

   与\ :hs:func:`~Data.Map.insert`\ 函数类似，但接受一个函数，当插入的键与原映射的键重复时，该函数负责处理重复的键对应的两个值。

   .. code-block::

      exp8 = M.insertWith (+) 3 100 $ M.fromList [(3, 4), (5, 103)]
             -- fromList [(3,104),(5,103)]

删除
====

.. hs:function:: delete :: Ord k => k -> Map k a -> Map k a

   接受一个键和一个映射，删除映射中的键及其对应值并返回新映射。

   .. code-block::

      exp9 = M.delete 5 $ M.fromList [(5, 'a'), (3, 'b')]
             -- fromList [(3,'b')]

.. hs:function:: deleteAt :: Int -> Map k a -> Map k a

   接受一个索引值（从 0 开始）和一个映射，删除对应索引的键值对，超过索引值则报错。

   .. code-block::

      exp10 = M.deleteAt 0 $ M.fromList [(1, 'a'), (2, 'b')]
              -- fromList [(2,'b')]

判断
====

.. hs:function:: null :: Map k a -> Bool

   判断映射是否为空。

   .. code-block::

      exp11 = M.null M.empty -- True
      exp12 = M.null $ M.insert 3 100 M.empty -- False

.. hs:function:: member :: Ord k => k -> Map k a -> Bool

   判断键是否存在于映射中。

   .. code-block::

      exp13 = M.member 3 $ M.fromList [(3, 6), (4, 5)] -- True

统计
====

.. hs:function:: size :: Map k a -> Int

   返回映射的键值对个数。

   .. code-block::

      exp14 = M.size $ M.insert 1 2 M.empty -- 1

重构
====

.. hs:function:: map :: (a -> b) -> Map k a -> Map k b

   与\ :hs:func:`GHC.Base.map`\ 函数类似，但作用于每个键值对的值。

   .. code-block::

      exp15 = M.map (*100) $ M.fromList [(1, 1), (2, 4)]
              -- fromlist [(1,100),(2,400)]

.. hs:function:: filter :: (a -> Bool) -> Map k a -> Map k a

   与\ :hs:func:`GHC.List.filter`\ 函数类似，但作用于每个键值对的值。

   .. code-block::

      exp16 = M.filter isUpper $ M.fromList [(1, 'a'), (2, 'A'), (3, 'b')]
              -- fromList [(2,'A')]

.. hs:function:: toList :: Map k a -> [(k, a)]

   将映射转换为序对列表。

   .. code-block::

      exp17 = M.toList . M.insert 9 2 $ M.singleton 4 3
              -- [(4,3),(9,2)]
