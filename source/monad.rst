.. highlight:: haskell
   :linenothreshold: 10

====
单子
====

简介
====

- :tr:`单子 (monad)`\ ：一种设计模式，将多个程序片段绑定在一起，并将结果封装到计算上下文中；
- 单子实际上是适用函子的加强版；

单子类型类
==========

.. code-block::

   class Applicative m => Monad m where
     return :: a -> m a
     (>>=) :: m a -> (a -> m b) -> m b
     (>>) :: m a -> m b -> m b

- ``Monad``\ 类型类：任何单子都是适用函子，且定义了上述三种方法；
- ``Monad``\ 类型类只接受\ :ref:`种类 <type-class:种类>`\ 为\ ``* -> *``\ 的类型构造器；

.. hs:module:: GHC.Base

单子方法
========

.. hs:function:: return :: Monad m => a -> m a

   接受一个值，将值打包进单子中，返回一个单子。

   该函数和\ :hs:func:`~GHC.Base.pure`\ 函数实际上是同一函数。

   .. collapse:: 源码

      .. code-block::
         :linenos:

         return :: a -> m a
         return = pure

.. hs:function:: (>>=) :: Monad m => m a -> (a -> m b) -> m b

   读作\ :tr:`绑定 (bind)`\ 。接受一个单子和一个函数，该函数接受一个值并返回另一个单子，对单子应用该函数后返回该函数返回的单子。

   可以理解为将第一个单子中的值取出，对该值应用函数后获得新的单子。

   .. code-block::

      exp1 = Just 2 >>= (\x -> return $ x + 1)           -- Just 3
      exp2 = Right "Hello" >>= (\x -> return $ x ++ "!") -- Right "Hello!"

.. hs:function:: (>>) :: Monad m => m a -> m b -> m b

   接受两个单子，对两个单子顺序求值，丢弃第一个单子的返回值，保留第二个单子的返回值。

   .. code-block::

      exp3 = Just 4 >> return 5 -- Just 5
      exp4 = getLine >> getLine
             -- Alice
             -- Amber
             -- "Amber"

   .. collapse:: 源码

      .. code-block::
         :linenos:

         (>>) :: forall a b. m a -> m b -> m b
         m >> k = m >>= \_ -> k

单子成员
========

- ``[]``

  - :ref:`与适用函子类似 <applicative-list>`\ ，由于列表为非确定性的，\ ``<-``\ 操作符会遍历列表所有元素并应用函数，最后提取所有结果中的元素为一个大列表；

    .. code-block::

       exp1 = [1, 2, 3] >>= \x -> return $ x + 1 -- [2,3,4]
       exp2 = [1, 2, 3] >> [1]                   -- [1,1,1]

  - 列表推导式实际上是列表单子语法的语法糖，列表推导式和\ ``do``\ :ref:`表示法 <monad:\`\`do\`\`\\ 表示法>`\ 最终都会翻译为\ :hs:func:`>>= <GHC.Base.(>>=)>`\ 运算符和匿名函数；

    .. code-block::

       exp3 = [ (n, ch) | n <- [1, 2], ch <- ['a', 'b'] ]
       exp4 = do
           n <- [1, 2]
           ch <- ['a', 'b']
           return (n, ch)
       exp5 = [1, 2] >>= \n -> ['a', 'b'] >>= \ch -> return (n, ch)

  .. code-block::

     type KnightPos = (Int, Int)

     -- | 在 8 * 8 大小的国际象棋棋盘上移动骑士。
     moveKnight :: KnightPos -> [KnightPos]
     moveKnight (c, r) = filter
         (\(c, r) -> c `elem` [1 .. 8] && r `elem` [1 .. 8])
         [ (c + 1, r + 2)
         , (c + 1, r - 2)
         , (c - 1, r + 2)
         , (c - 1, r - 2)
         , (c + 2, r + 1)
         , (c + 2, r - 1)
         , (c - 2, r + 1)
         , (c - 2, r - 1)
         ]

     -- | 移动骑士 3 次，返回第 3 次所有可能的坐标。
     in3 :: KnightPos -> [KnightPos]
     in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

     -- | 判断骑士从特定坐标出发并移动 3 次后是否能到达指定坐标。
     --
     -- ==== __例子：__
     -- >>> (6, 2) `canReachIn3` (6, 1)
     -- True
     --
     -- >>> (6, 2) `canReachIn3` (7, 3)
     -- False
     canReachIn3 :: KnightPos -> KnightPos -> Bool
     canReachIn3 start dest = dest `elem` in3 start

  .. collapse:: 源码

     .. code-block::
        :linenos:

        instance Monad [] where
            {-# INLINE (>>=) #-}
            xs >>= f = [ y | x <- xs, y <- f x ]
            {-# INLINE (>>) #-}
            (>>) = (*>)

- ``Maybe``

  - 若为\ ``Nothing``\ ，则返回\ ``Nothing``\ ，否则对单子应用函数；

    .. code-block::

       exp6 = return "WHAT" :: Maybe String    -- Just "WHAT"
       exp7 = Just 9 >>= \x -> return $ x * 10 -- Just 90

  .. code-block::

     type Birds = Int
     type Pole = (Birds, Birds)

     -- | 指定数量的鸟停在杆子左侧。
     -- 若左右鸟的数量差大于 3，则拿杆人失去平衡。
     landLeft :: Birds -> Pole -> Maybe Pole
     landLeft n (l, r) | abs (l + n - r) < 4 = Just (l + n, r)
                       | otherwise           = Nothing

     -- | 指定数量的鸟停在杆子右侧。
     -- 若左右鸟的数量差大于 3，则拿杆人失去平衡。
     landRight :: Birds -> Pole -> Maybe Pole
     landRight n (l, r) | abs (r + n - l) < 4 = Just (l, r + n)
                        | otherwise           = Nothing

     -- | 若拿杆人踩到香蕉皮，则立即失去平衡。
     banana :: Pole -> Maybe Pole
     banana _ = Nothing

     -- >>> routines
     -- [Nothing,Just (4,2),Nothing,Nothing]
     routines :: [Maybe Pole]
     routines =
         [ return (0, 0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1)
         , return (0, 0) >>= landLeft 2 >>= landRight 2 >>= landLeft 2
         , return (0, 0) >>= landLeft 1 >>= banana >>= landRight 1
         , return (0, 0) >>= landLeft 1 >> Nothing >>= landRight 1
         ]

  .. collapse:: 源码

     .. code-block::
        :linenos:

        instance Monad Maybe where
            (Just x) >>= k = k x
            Nothing  >>= _ = Nothing

            (>>) = (*>)

- ``IO``

  .. code-block::

     exp8 = getLine >>= readFile >>= putStrLn
            -- 输入文件名，并打印文件内容

- ``Either e``

  .. code-block::

     exp9 = Left "Error" >>= undefined -- Left "Error"

  .. collapse:: 源码

     .. code-block::
        :linenos:

        instance Monad (Either e) where
            Left  l >>= _ = Left l
            Right r >>= k = k r

- ``(->) r``

  .. code-block::

     exp10 = (+ 1) >>= (\x -> return $ x * 2) $ 3 -- 8

  .. collapse:: 源码

     .. code-block::
        :linenos:

        instance Monad ((->) r) where
            f >>= k = \ r -> k (f r) r

单子规则
========

.. rubric:: 规则一：\ :tr:`左同等 (Left identity)`

.. code-block::

   return a >>= k = k a

.. code-block::

   exp1 = return 3 >>= \x -> Just (x + 1) -- Just 4
   exp2 = (\x -> Just (x + 1)) 3          -- Just 4
   exp3 = return "H" >>= \x -> [x, x, x]  -- ["H", "H", "H"]
   exp4 = (\x -> [x, x, x]) "H"           -- ["H", "H", "H"]

.. rubric:: 规则二：\ :tr:`右同等 (Right identity)`

.. code-block::

   m >>= return = m

.. code-block::

   exp5 = Just 3 >>= return  -- Just 3
   exp6 = "Hello" >>= return -- "Hello"

.. rubric:: 规则三：\ :tr:`结合 (Associativity)`

.. code-block::

   m >>= (\x -> k x >>= h) = (m >>= k) >>= h

.. code-block::

   exp7 = (getLine >>= readFile) >>= putStrLn
   exp8 = getLine >>= (\fname -> readFile fname >>= putStrLn)

``do``\ 表示法
==============

.. code-block::

   do
   <variable> <- <Monad>
   <Monad>
   ...

   do { <variable> <- <Monad>; <Monad>; ... }

- ``do``\ :tr:`表示法 (notation)`：\ ``do``\ :ref:`语句块 <io:\`\`do\`\`\\ 语句块>`\ 除了能用于输入输出外，还能用于所有单子，可将多个单子操作链接起来；
- ``do``\ 表示法实际是单子语法的语法糖；

  - 将\ :hs:func:`>>= <GHC.Base.(>>=)>`\ 运算符和匿名函数简化为\ ``<-``\ 操作符；

    .. code-block::

       foo :: Maybe String
       foo = Just 3   >>= (\x ->
             Just "!" >>= (\y ->
             Just (show x ++ y)))

       foo' :: Maybe String
       foo' = do
           x <- Just 3
           y <- Just "!"
           Just (show x ++ y)

  - 将\ :hs:func:`>> <GHC.Base.(>>)>`\ 运算符省略；

    .. code-block::
       :name: sequential

       foo :: Maybe Int
       foo = Just 3  >>
             Nothing >>= (\x ->
             return $ x + 1) -- Nothing

       foo' :: Maybe Int
       foo' = do
           x <- Just 3
           Nothing -- 不使用 @<-@ 绑定变量，则效果同 @>>@
                   -- 和 @_ <- Nothing@ 等效，但更简洁
           return $ x + 1 -- Nothing

- ``do``\ 表示法中一行书写一个单子，最后一行不能使用\ ``<-``\ 运算符，因为将\ ``do``\ 表示法翻译回\ :hs:func:`>>= <GHC.Base.(>>=)>`\ 运算符和匿名函数就能发现，最后一个单子的结果代表整个\ ``do``\ 表示法的结果；
- ``do``\ 表示法也可以将所有单子书写在一行，单子间用分号分隔，但不推荐这种格式；

  .. code-block::

     foo :: Maybe Int
     foo = do
         x <- Just 3
         y <- Just 4
         return $ x + y -- Just 7

     foo' :: Maybe Int
     foo' = do { x <- Just 3; y <- Just 4; return $ x + y } -- Just 7

- ``do``\ 表示法允许模式匹配，模式匹配失败会调用\ :hs:mod:`Control.Monad.Fail.fail`\ 函数；

  .. code-block::

     foo :: Maybe Char
     foo = Just "Hello" >>= \(x : _) -> return x -- Just 'H'

     foo' :: Maybe Char
     foo' = do
         (x : _) <- Just "Hello"
         return x -- Just 'H'

     failedFoo' :: Maybe Char
     failedFoo' = do
         (x : _) <- Just ""
         return x -- Nothing

  .. collapse:: 源码

     .. code-block::
        :linenos:

        {-# LANGUAGE Trustworthy #-}
        {-# LANGUAGE NoImplicitPrelude #-}

        module Control.Monad.Fail ( MonadFail(fail) ) where

        import GHC.Base (String, Monad(), Maybe(Nothing), IO(), failIO)

        class Monad m => MonadFail m where
            fail :: String -> m a


        instance MonadFail Maybe where
            fail _ = Nothing

        instance MonadFail [] where
            {-# INLINE fail #-}
            fail _ = []

        instance MonadFail IO where
            fail = failIO

- ``do``\ 表示法虽然看上去与命令式语言相似，但实际上只是\ **顺序求值**\ ，前一行的求值结果会\ :ref:`影响后一行 <sequential>`\ ；
