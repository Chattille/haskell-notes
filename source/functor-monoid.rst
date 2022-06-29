.. highlight:: haskell
   :linenothreshold: 10

========================
函子、适用函子与单位半群
========================

函子
====

- :tr:`函子 (functor)`\ ：\ ``Functor``\ 类型类的成员，即可映射对象，能对其内的每个元素进行变换；
- 可以将函子视作在某个环境下输出特定值的类型（\ ``Maybe``\ 、\ ``[]``\ 、函数等）；

函子类型类
----------

.. code-block::

   class Functor f where
     fmap :: (a -> b) -> f a -> f b
     (<$) :: a -> f b -> f a

- ``Functor``\ 类型类：函子类型类，实现了\ :hs:func:`~GHC.Base.fmap`\ 函数和\ :hs:func:`<$ <GHC.Base.(<$)>`\ 运算符，函子是该类型类的成员，但该类型类的成员不一定都是\ :ref:`合格的函子 <functor-monoid:函子规则>`\ ；

  .. code-block::

     instance Functor Maybe where
       fmap f (Just x) = Just (f x)
       fmap f Nothing  = Nothing
         -- 因此 fmap :: (a -> b) -> Maybe a -> Maybe b

     instance Functor (Either a) where
       fmap f (Right x) = Right (f x)
       fmap f (Left x)  = Left x
         -- 因此 fmap :: (b -> c) -> Either a b -> Either a c

     import qualified Data.Map as M
     instance Functor (M.Map k) where
       fmap f v = map f v
         -- 因此 fmap :: (a -> b) -> M.Map k a -> M.Map k b

- ``Functor``\ 类型类只接受\ :ref:`种类 <type-class:种类>`\ 为\ ``* -> *``\ 的类型构造器，不符合的构造器可先部分应用；

  .. code-block::

     -- | 'Test' 种类为 @(* -> *) -> * -> * -> *@。
     data Test a b c = Test
       { fir  :: c  -- ^ @c@ 为具体类型。
       , sec :: a b -- ^ @b@ 为具体类型，@a b@ 相同。
                    -- 因此 @a@ 种类为 @* -> *@
       }

     -- | 部分应用 'Test' 后种类为 @* -> *@。
     instance Functor (Test a b) where
       fmap f (Test { fir = x, sec = y }) = Test { fir = f x, sec = y }

.. hs:module:: GHC.Base

函子方法
--------

.. hs:function:: fmap :: Functor f => (a -> b) -> f a -> f b

.. hs:function:: (<$>) :: Functor f => (a -> b) -> f a -> f b

   接受一个函数和一个函子，对函子包含的每个值应用函数并返回新函子。

   :hs:func:`<$> <(<$>)>`\ 中缀运算符是\ :hs:func:`fmap`\ 函数的同义词，其命名是对\ :hs:func:`$ <GHC.Base.($)>`\ 运算符的化用。

   .. note::

      可以将函子想象为容器，函数应用于容器中的值，再将结果放回容器。但要注意，“容器”只是一种比喻说法，更准确的术语为“\ :tr:`计算上下文 (computational context)`\ ”。

   .. code-block::

      exp1 = show (Just 3)      -- "Just 3"
      exp2 = fmap show (Just 3) -- Just "3"

.. hs:function:: (<$) :: Functor f => a -> f b -> f a

   接受一个值和一个函子，将函子内的每个值都替换为该值并返回新函子。

   .. code-block::

      exp3 = 3 <$ Just 1     -- Just 3
      exp4 = 'a' <$ [1 .. 4] -- "aaaa"

   .. collapse:: 源码

      .. code-block::
         :linenos:

         infixl 4 <$

         (<$) :: a -> f b -> f a
         (<$) = fmap . const

函子成员
--------

- ``Either a``

  .. code-block::

     exp1 = fmap (+ 100) (Right 2) -- Right 102
     exp2 = 100 <$ Right 2         -- Right 100
     exp3 = fmap (+ 100) (Left 2)  -- Left 2
     exp4 = 100 <$ Left 2          -- Left 2

  .. collapse:: 源码

     .. code-block::
        :linenos:

        instance Functor (Either a) where
            fmap _ (Left x) = Left x
            fmap f (Right y) = Right (f y)

- ``Maybe``

  .. code-block::

     exp5 = fmap (+ 100) (Just 2) -- Just 102
     exp6 = fmap (+ 100) Nothing  -- Nothing
     exp7 = 100 <$ Just 2         -- Just 100
     exp8 = 100 <$ Nothing        -- Nothing

  .. collapse:: 源码

     .. code-block::
        :linenos:

        instance Functor Maybe where
            fmap _ Nothing  = Nothing
            fmap f (Just a) = Just (f a)

- ``[]``

  - 根据函子类型类的定义可得\ :hs:func:`fmap`\ 函数应用于列表时的类型；

    .. code-block::

       fmap :: (a -> b) -> [] a -> [] b
       fmap :: (a -> b) -> [a] -> [b]
       map  :: (a -> b) -> [a] -> [b]

  - 由上一条可知，若\ :hs:func:`fmap`\ 函数应用于列表，则\ ``fmap = map``\ ；

    .. code-block::

       exp9  = fmap (+ 100) [1 .. 3] -- [101,102,103]
       exp10 = 100 <$ [1 .. 3]       -- [100,100,100]

  .. collapse:: 源码

     .. code-block::
        :linenos:

        instance Functor [] where
            fmap = map

- ``IO``

  .. code-block::

     import Data.Char

     -- | 将输入字符串转换为大写并逆序输出。
     main = do
       -- getLine :: IO String
       -- 因此 'f' 应用于 'String',
       -- 即 'IO (f String)'
       line <- fmap (reverse . map toUpper) getLine
       putStrLn line
       -- Alice in Wonderland
       -- DNALREDNOW NI ECILA

  .. collapse:: 源码

     .. code-block::
        :linenos:

        instance Functor IO where
            fmap f x = x >>= (pure . f)

- ``(->) r``

  - ``->``\ 的实质也是类型类，接受两个类型参数，因此所有函数都是函子；
  - 可以将函数想象成包含了返回值的容器；
  - 根据函子类型类的定义可得\ :hs:func:`fmap`\ 函数应用于函数时的类型；

    .. code-block::

       fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
       fmap :: (a -> b) -> (r -> a) -> r -> b
       (.)  :: (b -> c) -> (a -> b) -> a -> c

  - 由上一条可知，若\ :hs:func:`fmap`\ 函数应用于函数，则\ ``fmap = (.)``\ ；

    .. code-block::

       ghci> :t fmap (* 3) (+ 100)
       fmap (* 3) (+ 100) :: Num b => b -> b
       ghci> fmap (* 3) (+ 100) 1
       303
       ghci> (* 3) `fmap` (+ 100) $ 1
       303
       ghci> (* 3) . (+ 100) $ 1
       303

  .. collapse:: 源码

     .. code-block::
        :linenos:

        instance Functor ((->) r) where
            fmap = (.)

提升
----

- :tr:`提升 (lifting)`\ ：在更通用的环境中，将一个函数转换为另一个函数；
- :hs:func:`fmap`\ 函数的类型声明可以理解为\ ``fmap :: Functor f => (a -> b) -> (f a -> f b)``\ ，即原本只能作用于普通值的一元函数，提升后可以作用于函子；

.. code-block::

   ghci> :t fmap (* 100)
   fmap (* 100) :: (Functor f, Num b) => f b -> f b
   ghci> :t fmap (replicate 3)
   fmap (replicate 3) :: Functor f => f a -> f [a]
   ghci> mapReverse = fmap reverse
   ghci> :t mapReverse
   mapReverse :: Functor f => f [a] -> f [a]
   ghci> -- 提升后的函数可应用于函子
   ghci> plusOne = fmap (+ 1)
   ghci> plusOne $ Just 2
   Just 3
   ghci> plusOne [1 .. 3]
   [2,3,4]
   ghci> plusOne $ Right 2
   Right 3

函子规则
--------

.. note::

   该两条规则和数学中函子的定义是一致的。

- 为确保代码的可扩展性和抽象性，\ :hs:func:`fmap`\ 函数对函子应该只做映射操作，因此所有函子都应该遵守两条规则，满足这两条规则的类型可以作为函子；
- 这两条规则 Haskell 并未强制实现，因此需要手动实现；
- 不满足这两条规则的\ ``Functor``\ 类型类成员可能导致不可预测的结果；

  .. code-block::

     data CMaybe a = CNothing | CJust Int a deriving Show

     instance Functor CMaybe where
       fmap f CNothing = CNothing
       fmap f (CJust counter x) = CJust (counter + 1) (f x)

     exp3 = fmap id (CJust 0 "alice") -- CJust 1 "alice"
     exp4 = id (Cjust 0 "alice")      -- CJust 0 "alice"

     exp5 = fmap (reverse . map succ) (CJust 0 "alice")
            -- CJust 1 "fdjmb"
     exp6 = fmap reverse . fmap (map succ) $ CJust 0 "alice"
            -- CJust 2 "fdjmb"

.. rubric:: 规则一：\ :tr:`同等 (Identity)`

.. code-block::

   fmap id == id

若\ :hs:func:`fmap`\ 函数将\ :hs:func:`id`\ 函数应用于函子，则返回结果应该与原函子完全相同。

.. code-block::

   instance Functor Maybe where
     fmap f (Just x) = Just (f x)
     -- fmap id (Just x) == Just (id x) == Just x == id Just x
     fmap f Nothing  = Nothing
     -- fmap id Nothing == Nothing == id Nothing

.. rubric:: 规则二：\ :tr:`组合 (Composition)`

.. code-block::

   fmap (f . g) == fmap f . fmap g

对函子应用函数组合后的结果，应该与按顺序应用所有函数后的结果相同。

.. code-block::

   exp1 = fmap (reverse . map succ) (Just "Alice")
          -- Just "fdjmB"
   exp2 = fmap reverse . fmap (map succ) $ Just "Alice"
          -- Just "fdjmB"

适用函子
========

- :tr:`适用函子 (applicative functor)`\ ：\ ``Applicative``\ 类型类的成员，函子的增强版；
- ``Functor``\ 类型类的方法不能处理两个普通函子间的运算，而适用函子类型类可以；

适用函子类型类
--------------

.. code-block::

   class Functor f => Applicative f where
     pure :: a -> f a
     (<*>) :: f (a -> b) -> f a -> f b
     liftA2 :: (a -> b -> c) -> f a -> f b -> f c
     (*>) :: f a -> f b -> f b
     (<*) :: f a -> f b -> f a

- ``Applicative``\ 类型类：定义了上述方法的类型类，其成员同时也是\ ``Functor``\ 类型类的成员；

适用函子方法
------------

.. hs:function:: pure :: Applicative f => a -> f a

   接受一个值，将值打包进函子中，返回一个适用函子。

   .. code-block::

      exp1 = pure 1 :: Maybe Int                  -- Just 1
      exp2 = pure "alice" :: Either String String -- Right "alice"

.. hs:function:: (<*>) :: Applicative f => f (a -> b) -> f a -> f b

   接受一个包含函数的函子和另一函子，将函数提取出来并应用于另一函子内的值，最终返回新函子。

   .. code-block::

      exp3 = Just (+ 3) <*> Just 9     -- Just 12
      exp4 = pure (+ 3) <*> Just 10    -- Just 13
      exp5 = Just (++ "!") <*> Nothing -- Nothing

   .. collapse:: 源码

      .. code-block::
         :linenos:

         infixl 4 <*>

         (<*>) :: f (a -> b) -> f a -> f b
         (<*>) = liftA2 id

.. hs:function:: liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c

   用于提升函数，即原本只能作用于普通值的二元函数，提升后可作用于两个函子。

   .. code-block::

      import GHC.Base
      exp6 = liftA2 (+) (Just 1) (Just 2) -- Just 3
      exp7 = liftA2 (*) (Just 3) Nothing  -- Nothing

   .. collapse:: 源码

      .. code-block::
         :linenos:

         liftA2 :: (a -> b -> c) -> f a -> f b -> f c
         liftA2 f x = (<*>) (fmap f x)

.. hs:function:: (*>) :: Applicative f => f a -> f b -> f b

   丢弃第一个参数，选择第二个参数。

   .. code-block::

      exp8 = Just 1 *> Just 2 -- Just 2

   .. collapse:: 源码

      .. code-block::
         :linenos:

         infixl 4 *>

         (*>) :: f a -> f b -> f b
         a1 *> a2 = (id <$ a1) <*> a2

.. hs:function:: (<*) :: Applicative f => f a -> f b -> f a

   丢弃第二个参数，选择第一个参数。

   .. code-block::

      exp9 = Just 1 <* Just 2 -- Just 1

   .. collapse:: 源码

      .. code-block::
         :linenos:

         infixl 4 <*

         (<*) :: f a -> f b -> f a
         (<*) = liftA2 const

适用函子成员
------------

- ``Either e``

  .. code-block::

     import GHC.Base
     exp1 = pure 1 :: Either e Int         -- Right 1
     exp2 = Right (+ 100) <*> Right 2      -- Right 102
     exp3 = liftA2 (+) (Right 3) (Right 4) -- Right 7

  .. collapse:: 源码

     .. code-block::
        :linenos:

        instance Applicative (Either e) where
            pure          = Right
            Left  e <*> _ = Left e
            Right f <*> r = fmap f r

.. _applicative-list:

- ``[]``

  - 由于列表的结果是\ :tr:`非确定 (non-deterministic)`\ 的，因此\ :hs:func:`<*> <GHC.Base.(<*>)>`\ 会遍历提取列表所有元素并应用于右侧列表所有元素上，返回的列表包含所有排列组合的结果；
  - \ :hs:func:`<*> <GHC.Base.(<*>)>`\ 可用来替代列表推导式；

    .. code-block::

       exp4 = [ x * y | x <- [2, 5, 10], y <- [8, 10, 11] ]
              -- [16,20,22,40,50,55,80,100,110]
       exp5 = (*) <$> [2, 5, 10] <*> [8, 10, 11]
              -- [16,20,22,40,50,55,80,100,110]

  - 对于列表，\ ``pure f <*> xs``\ 等价于\ ``fmap f xs``\ ；

  .. code-block::

     exp6 = [(+ 1),(+ 2)] <*> pure 4 -- [5,6]
     exp7 = liftA2 (*) [1 .. 3] [2 .. 4]
            -- [1*2,1*3,1*4,2*2,2*3,2*4,3*2,3*3,3*4] = [2,3,4,4,6,8,6,9,12]

  .. collapse:: 源码

     .. code-block::
        :linenos:

        instance Applicative [] where
            {-# INLINE pure #-}
            pure x    = [x]
            {-# INLINE (<*>) #-}
            fs <*> xs = [ f x | f <- fs, x <- xs ]
            {-# INLINE liftA2 #-}
            liftA2 f xs ys = [ f x y | x <- xs, y <- ys ]
            {-# INLINE (*>) #-}
            xs *> ys  = [ y | _ <- xs, y <- ys ]

- ``Maybe``

  .. code-block::

     exp8 = Just (+ 3) <*> Nothing -- Nothing

  .. collapse:: 源码

     .. code-block::
        :linenos:

        instance Applicative Maybe where
            pure = Just

            Just f  <*> m  = fmap f m
            Nothing <*> _m = Nothing

            liftA2 f (Just x) (Just y) = Just (f x y)
            liftA2 _ _ _ = Nothing

            Just _m1 *> m2  = m2
            Nothing  *> _m2 = Nothing

- ``IO``

  - 对于\ ``IO``\ 类，\ :hs:func:`pure`\ 等价于\ :hs:func:`return`\ ；

  .. code-block::

     exp9 <- pure (++ "!") <*> getLine
          -- Alice
          -- "Alice!"
     exp10 <- (++) <$> getLine <*> getLine
           -- Hello
           -- World
           -- "HelloWorld"

- ``(->) r``

  - 对于函数，\ :hs:func:`pure`\ 等价于\ :hs:func:`const`\ ；
  - 从左侧函数提取出返回值后应用于右侧函数的返回值，返回结果；

  .. code-block::

     exp11 = pure 3 "blah"       -- 3
     exp12 = (+) <*> (* 100) $ 2 -- (+2) (2*100) = 202

  .. collapse:: 源码

     .. code-block::
        :linenos:

        instance Applicative ((->) r) where
            pure = const
            (<*>) f g x = f x (g x)
            liftA2 q f g x = q (f x) (g x)

- ``Control.Applicative.ZipList``

  - 对普通列表使用\ :hs:func:`<*> <GHC.Base.(<*>)>`\ 时，结果可看作两个列表元素的排列组合，而对\ ``ZipList``\ 类型使用\ :hs:func:`<*> <GHC.Base.(<*>)>`\ 时，可对列表对应位置的元素进行操作（长度为最短列表的长度）；

    .. collapse:: 源码

       .. code-block::
          :linenos:

          newtype ZipList a = ZipList { getZipList :: [a] }
                            deriving ( Show     -- ^ @since 4.7.0.0
                                     , Eq       -- ^ @since 4.7.0.0
                                     , Ord      -- ^ @since 4.7.0.0
                                     , Read     -- ^ @since 4.7.0.0
                                     , Functor  -- ^ @since 2.01
                                     , Foldable -- ^ @since 4.9.0.0
                                     , Generic  -- ^ @since 4.7.0.0
                                     , Generic1 -- ^ @since 4.7.0.0
                                     )

  - 对\ ``ZipList``\ 类型应用\ :hs:func:`pure`\ 时返回无限列表，满足\ ``pure f <*> xs == fmap f xs``\ ；

  .. code-block::

     exp13 = getZipList $ (+) <$> ZipList [1, 2, 3] <*> ZipList [100, 100, 100]
             -- -> getZipList $ ZipList[(+1),(+2),(+3)] <*> ZipList [100,100,100]
             -- -> getZipList $ ZipList [((+1) 100),((+2) 100),((+3) 100)]
             -- -> [101,102,103]
     exp14 = getZipList $ pure (+ 3) <*> ZipList [1, 2, 3]
             -- -> getZipList $ ZipList [(+3),(+3),(+3),...] <*> ZipList [1,2,3]
             -- -> getZipList $ ZipList [((+3) 1),((+3) 2),((+3) 3)]
             -- -> [4,5,6]


  .. collapse:: 源码

     .. code-block::
        :linenos:

        instance Applicative ZipList where
            pure x = ZipList (repeat x)
            liftA2 f (ZipList xs) (ZipList ys) = ZipList (zipWith f xs ys)

适用函子规则
------------

- 和函子相同，适用函子同样需要遵循一定的规则；

.. rubric:: 规则一：\ :tr:`同等 (Identity)`

.. code-block::

   pure id <*> v = v

.. code-block::

   exp1 = pure id <*> Just 2 -- Just 2

.. rubric:: 规则二：\ :tr:`组合 (Composition)`

.. code-block::

   pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

.. code-block::

   exp2 = pure (.) <*> Just (+ 3) <*> Just (* 4) <*> Just 5 -- Just 23
   exp3 = Just (+ 3) <*> (Just (* 4) <*> Just 5)            -- Just 23

.. rubric:: 规则三：\ :tr:`同态 (Homomorphism)`

.. code-block::

   pure f <*> pure x = pure (f x)

.. code-block::

   exp4 = pure (+ 2) <*> pure 3 -- 5
   exp5 = pure ((+ 2) 3)        -- 5

.. rubric:: 规则四：\ :tr:`交换 (Interchange)`

.. code-block::

   u <*> pure y = pure ($ y) <*> u

.. code-block::

   exp6 = Just (+ 2) <*> pure 3     -- Just 5
   exp7 = pure ($ 3) <*> Just (+ 2) -- Just 5

单位半群
========

定义
----

- :tr:`半群 (semigroup)`\ ：对于非空集合 :math:`S` 和二元运算操作 :math:`\circ : S \times S \to S`\ ，若 :math:`\forall x, y, z \in S`\ ，满足 :math:`(x \circ y) \circ z = x \circ (y \circ z)`\ ，则称二元元组 :math:`(S, \circ)` 为半群，简称为半群 :math:`S`\ ；

  - 这种性质称为\ :tr:`结合性 (associativity)`\ ；

  .. note::

     对于自然数集 :math:`\mathbb{N}`，有乘法运算 :math:`\times`，且 :math:`\forall x, y ,z \in \mathbb{N}`，满足 :math:`(x \times y) \times z = x \times (y \times z)`，因此 :math:`(\mathbb{N}, \times)` 为半群。

- :tr:`单位半群 (monoid)`\ ：对于半群 :math:`S`\ ，若 :math:`\exists e`\ ，有 :math:`\forall a \in S`\ ，满足 :math:`a \circ e = e \circ a = a`\ ，则称三元元组 :math:`(S, \circ, e)` 为单位半群，也称为幺半群；

  - :math:`e` 称为\ :tr:`单位元 (identity)`\ ；

  .. note::

     对于半群 :math:`(\mathbb{N}, \times)`\ ，存在单位元 :math:`1`\ ，对于 :math:`\forall a \in \mathbb{N}`\ ，满足 :math:`a \times 1 = 1 \times a = a`\ ，因此 :math:`(\mathbb{N}, \times, 1)` 为单位半群。

- Haskell 中\ ``Semigroup``\ 和\ ``Monoid``\ 类型类的定义与数学定义基本一致；

半群类型类
----------

.. code-block::

   class Semigroup a where
     (<>) :: a -> a -> a
     sconcat :: NonEmpty a -> a
     stimes :: Integral b => b -> a -> a

- ``Semigroup``\ 类型类：接受一个具体类型；

.. hs:module:: GHC.Base

半群方法
--------

.. hs:function:: (<>) :: Semigroup a => a -> a -> a

   接受两个半群，以特定方式结合后返回第三个半群。

   .. collapse:: 源码

      .. code-block::
         :linenos:

         infixr 6 <>

.. hs:function:: sconcat :: Semigroup a => NonEmpty a -> a

   接受一个\ ``NonEmpty``\ 类型，对列表中所有值应用\ :hs:func:`<> <GHC.Base.(<>)>`\ 后将所有结果缩减为一个最终值。

.. hs:function:: stimes :: (Semigroup a, Integral b) => b -> a -> a

   接受一个整型值和一个半群，将半群中的值重复整型值次，返回结果半群。

   .. code-block::

      res = stimes 4 [1, 2] -- [1,2,1,2,1,2,1,2]

半群规则
--------

- Haskell 中\ ``Semigroup``\ 类型类的规则与数学定义基本一致

.. rubric:: 规则：\ :tr:`结合 (Associativity)`

.. code-block::

   x <> (y <> z) = (x <> y) <> z

单位半群类型类
--------------

.. code-block::

   class Semigroup a => Monoid a where
     mempty :: a
     mappend :: a -> a -> a
     mconcat :: [a] -> a

- ``Monoid``\ 类型类：接受一个具体类型，同时该类型也是\ ``Semigroup``\ 类型类的成员；

单位半群方法
------------

.. hs:function:: mempty :: Monoid a => a

   本质为\ :ref:`多态常量 <type:类型变量>`\ ，代表该单位半群的单位元。

   .. code-block::

      exp1 = mempty :: [a]        -- []
      exp2 = mempty :: String     -- ""
      exp3 = mempty :: ([a], [b]) -- ([],[])

.. hs:function:: mappend :: Monoid a => a -> a -> a

   该函数和\ :hs:func:`<> <GHC.Base.(<>)>`\ 运算符是同义词，但接受两个单位半群。

   .. code-block::

      exp4 = mappend [] [1]          -- [1]
      exp5 = mappend "Hello" "World" -- "HelloWorld"

   .. note::

      :hs:func:`mappend`\ 函数的命名稍微有些费解。虽然函数名带“append”，但并不代表该函数将两个值追加在一起。实际上该函数只是一个二元函数，接受两个值并返回第三个值。

   .. attention::

      该函数是\ :hs:func:`<> <GHC.Base.(<>)>`\ 运算符的同义词，因此稍显冗余。Haskell 会在未来版本中移除该函数。

   .. collapse:: 源码

      .. code-block::
         :linenos:

         mappend :: a -> a -> a
         mappend = (<>)
         {-# INLINE mappend #-}

.. hs:function:: mconcat :: Monoid a => [a] -> a

   接受单位半群值的列表，并对所有值应用\ :hs:func:`mappend`\ 函数，返回最终值。

   .. code-block::

      exp6 = mconcat ["Hello", "World"]     -- "HelloWorld"
      exp7 = mconcat [["Hello"], ["World"]] -- ["Hello", "World"]

   .. collapse:: 源码

      .. code-block::
         :linenos:

         mconcat :: [a] -> a
         mconcat = foldr mappend mempty
         {-# INLINE mconcat #-}

单位半群成员
------------

- ``[a]``

  .. code-block::

     exp1 = ("one" <> "two") <> "three"   -- "onetwothree"
     exp2 = "one" <> ("two" <> "three")   -- "onetwothree"
     exp3 = "one" <> mempty               -- "one"
     exp4 = mconcat [[1, 2], [3, 6], [9]] -- [1,2,3,6,9]
     exp5 = mempty :: [a]                 -- []

  .. collapse:: 源码

     .. code-block::
        :linenos:

        import Data.Semigroup.Internal (stimesList)

        instance Semigroup [a] where
                (<>) = (++)
                {-# INLINE (<>) #-}

                stimes = stimesList

        instance Monoid [a] where
                {-# INLINE mempty #-}
                mempty = []
                {-# INLINE mconcat #-}
                mconcat xss = [ x | xs <- xss, x <- xs ]

- ``Product a``

  - ``Product``\ 定义于\ :hs:mod:`Data.Semigroup`\ 模块，\ ``newtype``\ 数字类型，表示数字的乘积；

    .. collapse:: 源码

       .. code-block::
          :linenos:

          newtype Product a = Product { getProduct :: a }
                  deriving ( Eq       -- ^ @since 2.01
                           , Ord      -- ^ @since 2.01
                           , Read     -- ^ @since 2.01
                           , Show     -- ^ @since 2.01
                           , Bounded  -- ^ @since 2.01
                           , Generic  -- ^ @since 4.7.0.0
                           , Generic1 -- ^ @since 4.7.0.0
                           , Num      -- ^ @since 4.7.0.0
                           )

  - ``Product``\ 类型的二元运算为乘法，单位元为\ ``Product 1``\；

  .. code-block::

     exp6 = getProduct $ Product 2 <> Product 3         -- 6
     exp7 = getProduct . mconcat . map Product $ [1..5] -- 120
     exp8 = getProduct mempty                           -- 1

  .. collapse:: 源码
 
     .. code-block::
        :linenos:

        instance Num a => Semigroup (Product a) where
                (<>) = coerce ((*) :: a -> a -> a)
                stimes n (Product a) = Product (a ^ n)
 
        instance Num a => Monoid (Product a) where
                mempty = Product 1

- ``Sum a``

  - ``Sum``\ 定义于\ :hs:mod:`Data.Semigroup`\ 模块，\ ``newtype``\ 数字类型，表示数字的和；

    .. collapse:: 源码

       .. code-block::
          :linenos:

          newtype Sum a = Sum { getSum :: a }
                  deriving ( Eq       -- ^ @since 2.01
                           , Ord      -- ^ @since 2.01
                           , Read     -- ^ @since 2.01
                           , Show     -- ^ @since 2.01
                           , Bounded  -- ^ @since 2.01
                           , Generic  -- ^ @since 4.7.0.0
                           , Generic1 -- ^ @since 4.7.0.0
                           , Num      -- ^ @since 4.7.0.0
                           )

  - ``Sum``\ 类型的二元运算为加法，单位元为\ ``Sum 0``\；

  .. code-block::

    exp9  = getSum $ Sum 2 <> Sum 9      -- 11
    exp10 = getSum . mconcat . map Sum $ [1..5] -- 15

  .. collapse:: 源码
  
     .. code-block::
        :linenos:

        instance Num a => Semigroup (Sum a) where
                (<>) = coerce ((+) :: a -> a -> a)
                stimes n (Sum a) = Sum (fromIntegral n * a)
  
        instance Num a => Monoid (Sum a) where
                mempty = Sum 0

- ``Any``

  - 定义于\ :hs:mod:`Data.Semigroup`\ 模块，\ ``newtype``\ 布尔类型，表示存在一个布尔值为真；

    .. collapse:: 源码

       .. code-block::
          :linenos:

          newtype Any = Any { getAny :: Bool }
                  deriving ( Eq      -- ^ @since 2.01
                           , Ord     -- ^ @since 2.01
                           , Read    -- ^ @since 2.01
                           , Show    -- ^ @since 2.01
                           , Bounded -- ^ @since 2.01
                           , Generic -- ^ @since 4.7.0.0
                           )

  - ``Any``\ 类型的二元运算为逻辑或，单位元为\ ``False``；

  .. code-block::

     exp11 = getAny $ Any True <> Any False             -- True
     exp12 = getAny . mconcat . map Any $ [False, True, False] -- True

  .. collapse:: 源码

     .. code-block::
        :linenos:

        instance Semigroup Any where
                (<>) = coerce (||)
                stimes = stimesIdempotentMonoid

        instance Monoid Any where
                mempty = Any False

- ``All``

  - 定义于\ :hs:mod:`Data.Semigroup`\ 模块，\ ``newtype``\ 布尔类型，表示所有布尔值均为真；

    .. collapse:: 源码

       .. code-block::
          :linenos:

          newtype All = All { getAll :: Bool }
                  deriving ( Eq      -- ^ @since 2.01
                           , Ord     -- ^ @since 2.01
                           , Read    -- ^ @since 2.01
                           , Show    -- ^ @since 2.01
                           , Bounded -- ^ @since 2.01
                           , Generic -- ^ @since 4.7.0.0
                           )

  - ``All``\ 类型的二元运算为逻辑和，单位元为\ ``All True``\ ；

  .. code-block::

     exp13 = getAll $ All True <> All False           -- False
     exp14 = getAll . mconcat . map All $ [True, True, True] -- True

  .. collapse:: 源码

     .. code-block::
        :linenos:

        instance Semigroup All where
                (<>) = coerce (&&)
                stimes = stimesIdempotentMonoid

        instance Monoid All where
                mempty = All True

- ``Ordering``

  - ``Ordering``\ 类型也是\ ``Monoid``\ 类型类的成员，单位元为\ ``EQ``\ ；
  - ``Ordering``\ 类型的二元运算始终保留左侧值，除非左侧值为\ ``EQ``\ ；

    .. code-block::

       exp15 = (LT <> GT) <> GT -- LT
       exp16 = LT <> (GT <> GT) -- LT
       exp17 = EQ <> GT         -- GT
       exp18 = GT <> EQ         -- GT

  - ``Ordering``\ 类型作为\ ``Monoid``\ 类型类的成员，让对象间的比较方式更加丰富，且允许按照重要程度对比较方式进行排序；

  .. tip::

     可以将\ ``Ordering``\ 的单位半群想象为英语词典词条排序的过程。若两个词条的首字母不同（\ ``LT``\ 或\ ``GT``\ ），则比较结束，直接返回结果；若首字母相同（\ ``EQ``\ ），则比较下一字母。

  .. code-block::

     -- | 比较两个字符串长度。
     -- 若长度相同，则比较元音字母数量。
     -- 若元音数量相同，则比较字母顺序。
     --
     -- ==== __例子：__
     -- >>> lengthCompare "o" "on"
     -- LT
     --
     -- >>> lengthCompare "zen" "ana"
     -- LT
     --
     -- >>> lengthCompare "ox" "on"
     -- GT
     lengthCompare :: String -> String -> Ordering
     lengthCompare x y =
         (length x `compare` length y)
             <> (vowels x `compare` vowels y)
             <> (x `compare` y)
         where vowels = length . filter (`elem` "aeiou")

  .. collapse:: 源码

     .. code-block::
        :linenos:

        instance Semigroup Ordering where
                LT <> _ = LT
                EQ <> y = y
                GT <> _ = GT

                stimes = stimesIdempotentMonoid

        instance Monoid Ordering where
                mempty = EQ

- ``Maybe a``

  - ``Maybe``\ 类型的单位元为\ ``Nothing``\ ；
  - 始终对\ ``Maybe``\ 类型的值应用\ :hs:func:`mappend`\ 函数，除非值为\ ``Nothing``\ ，此时保留另一个值；
  - ``Maybe``\ 类型的类型参数也必须为\ ``Semigroup``\ 类型类的成员；

    .. code-block::

       exp19 = Just "alice" <> Nothing      -- Just "alice"
       exp20 = Nothing <> Just LT           -- Just LT
       exp21 = Just (Sum 3) <> Just (Sum 4) -- Just (Sum {getSum = 7})

  .. collapse:: 源码

     .. code-block::
        :linenos:

        instance Semigroup a => Semigroup (Maybe a) where
            Nothing <> b       = b
            a       <> Nothing = a
            Just a  <> Just b  = Just (a <> b)

            stimes = stimesMaybe

        instance Semigroup a => Monoid (Maybe a) where
            mempty = Nothing

- ``First a``

  - ``First``\ 定义于\ :hs:mod:`Data.Monoid`\ 模块，\ ``newtype`` ``Maybe``\ 类型，表示最左侧的非\ ``Nothing``\ 值；

    .. collapse:: 源码

       .. code-block::
          :linenos:

          newtype First a = First { getFirst :: Maybe a }
                  deriving ( Eq          -- ^ @since 2.01
                           , Ord         -- ^ @since 2.01
                           , Read        -- ^ @since 2.01
                           , Show        -- ^ @since 2.01
                           , Generic     -- ^ @since 4.7.0.0
                           , Generic1    -- ^ @since 4.7.0.0
                           , Functor     -- ^ @since 4.8.0.0
                           , Applicative -- ^ @since 4.8.0.0
                           , Monad       -- ^ @since 4.8.0.0
                           )

  - ``First``\ 类型保留最左侧的值，若左值为\ ``Nothing``\ ，则保留右值，单位元为\ ``First Nothing``\ ；

  .. code-block::

     exp22 = getFirst $ First (Just 'a') <> First (Just 'b')    -- Just 'a'
     exp23 = getFirst . mconcat . map First $ [Nothing, Just 1] -- Just 1

  .. collapse:: 源码

     .. code-block::
        :linenos:

        instance Semigroup (First a) where
                First Nothing <> b = b
                a             <> _ = a
                stimes = stimesIdempotentMonoid

        instance Monoid (First a) where
                mempty = First Nothing

- ``Last a``

  - ``Last``\ 定义于\ ``Data.Monoid``\ 模块，``newtype`` ``Maybe``\ 类型，表示最右侧的非\ ``Nothing``\ 值；

    .. collapse:: 源码

       .. code-block::
          :linenos:

          newtype Last a = Last { getLast :: Maybe a }
                  deriving ( Eq          -- ^ @since 2.01
                           , Ord         -- ^ @since 2.01
                           , Read        -- ^ @since 2.01
                           , Show        -- ^ @since 2.01
                           , Generic     -- ^ @since 4.7.0.0
                           , Generic1    -- ^ @since 4.7.0.0
                           , Functor     -- ^ @since 4.8.0.0
                           , Applicative -- ^ @since 4.8.0.0
                           , Monad       -- ^ @since 4.8.0.0
                           )

  - ``Last``\ 类型保留最右侧的值，若右值为\ ``Nothing``\ ，则保留左值，单位元为\ ``Last Nothing``\ ；

  .. code-block::

     exp24 = getLast $ Last (Just 1) <> Last (Just 2)                 -- Just 2
     exp25 = getLast . mconcat . map Last $ [Just 1, Just 2, Nothing] -- Just 2

  .. collapse:: 源码

     .. code-block::
        :linenos:

        instance Semigroup (Last a) where
                a <> Last Nothing = a
                _ <> b            = b
                stimes = stimesIdempotentMonoid

        instance Monoid (Last a) where
                mempty = Last Nothing

单位半群规则
------------

- Haskell 中\ ``Monoid``\ 类型类的规则与\ :ref:`数学定义 <functor-monoid:定义>`\ 基本一致；

.. rubric:: 规则一：\ :tr:`结合 (Associativity)`

.. code-block::

   (x <> y) <> z = x <> (y <> z)

.. rubric:: 规则二：\ :tr:`右同等 (Right identity)`

.. code-block::

   x <> mempty = x

.. rubric:: 规则三：\ :tr:`左同等 (Left identity)`

.. code-block::

   mempty <> x = x

.. rubric:: 规则四：\ :tr:`串联 (Concatenation)`

.. code-block::

   mconcat = foldr (<>) mempty
