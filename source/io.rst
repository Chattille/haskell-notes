.. highlight:: haskell
   :linenothreshold: 10

========
输入输出
========

编译
====

.. code-block:: console

   $ ghc [options] file ...
   $ runhaskell [options] file ...
   $ runghc [options] file ...

- GHC 编译器可对 Haskell 文件进行编译，生成中间文件和可执行文件；
- ``runhaskell``\ 命令为 GHC 自带，可实时编译 Haskell 文件，且不会生成任何文件；
- ``runghc``\ 命令是\ ``runhaskell``\ 命令的同义词；

输出
====

- Haskell 是无副作用的，即不改变对象的状态，为了和外界交互，Haskell 提供了一套处理有副作用函数的系统；

.. hs:module:: System.IO

.. hs:function:: putStrLn :: String -> IO ()

   将字符串输出到标准输出，并在末尾添加换行符。

   ``IO``\ 类型类为 I/O 相关类型类，接受一个类型参数\ ``a``\ ，该类在执行时先执行输入输出操作（有副作用，通常为从键盘获得值或将值输出到屏幕），再返回类型为\ ``a``\ 的值（类似于返回值）。

   ``IO``\ 类型类只能在\ ``main``\ 变量中、其他 I/O 语句中或 GHCi 环境中执行，因此\ ``main``\ 变量的类型始终为\ ``IO <type>``\ ，所以通常不会声明\ ``main``\ 变量的类型。

   .. tabs::

      .. tab:: Test.hs

         .. code-block::

            main :: IO ()
            main = putStrLn "Hello, world!"

      .. tab:: 控制台

         .. code-block:: console

            $ ghc --make Test
            [1 of 1] Compiling Main             ( Test.hs, Test.o )
            Linking Test ...
            $ ./Test
            Hello, world!

输入
====

.. hs:function:: getLine :: IO String

   :hs:func:`getLine`\ 函数类型声明为\ ``IO String``\ ，表示从键盘读取一行字符串并返回结果为\ ``IO String``\ ，不包括换行符。

   ``<-``\ 将\ ``IO``\ 类型类执行的结果与变量绑定，会将\ ``IO <type>``\ 类型转换为\ ``<type>``\ （取出具体类型）并与变量绑定。

   ``<-``\ 可将有副作用和无副作用的代码分隔，对\ :hs:func:`putStrLn`\ 同样有效。

   .. code-block::

      Prelude> getLine
      alice
      "alice"

``do``\ 语句块
==============

.. code-block::

   do <IO action>; ...

- ``do``\ 语句块将多个 I/O 语句结合在一起，从上至下执行（类似命令式语言）；
- ``do``\ 语句块最后一个语句不能使用\ ``<-``\ 绑定变量，因为\ ``do``\ 语句块会从最后一个语句获得返回值并将其绑定在自己的变量上；
- 单行书写时，语句之间用分号分隔，多行书写不用分号；
- ``let``\ 语句同样适用于\ ``do``\ 语句块，且\ ``in``\ 语句可省略；

  .. code-block::

     import qualified Data.Char as Ch

     main :: IO ()
     main = do
         putStrLn "What is your first name?"
         firstName <- getLine
         putStrLn "What is your last name?"
         lastName <- getLine
         let upperFirstName = map Ch.toUpper firstName
             upperLastName  = map Ch.toUpper lastName
         putStrLn $ "Hello " ++ upperFirstName ++ " " ++ upperLastName

- ``do``\ 语句块实际上是\ :ref:`单子的语法糖 <monad:\`\`do\`\`\\ 表示法>`\ ；

.. code-block::

   main :: IO ()
   main = do
       putStrLn "What is your name?"
       name <- getLine
       putStrLn $ "Helle, " ++ name ++ "!"

``return``\ 函数
================

.. hs:function:: return :: Monad m => a -> m a
   :module:

- :hs:func:`return`\ 函数与命令式语言中的\ ``return``\ 关键字不同，表示将参数返回为\ :doc:`单子 <monad>`\ 而值不变；
- :hs:func:`return`\ 函数在 I/O 中无实际意义，通常用于创建\ ``IO``\ 类型类的返回值；

.. code-block::

   -- | 将每个单词反序输出。
   main :: IO ()
   main = do
       line <- getLine
       if null line
           then return () -- if 语句具有相同返回值
           else do
               putStrLn $ reverseWords line
               main       -- 递归调用

   -- | 反序输出字符串中的每个单词。
   reverseWords :: String -> String
   reverseWords = unwords . map reverse . words

I/O 函数
========

输出函数
--------

.. hs:function:: putStr :: String -> IO ()

   将字符串写入标准输出，但没有换行符。

   .. code-block::
 
      main :: IO ()
      main = do
          putStr "Hey, "
          putStr "I'm "
          putStrLn "Andy!" -- Hey, I'm Andy!

.. hs:function:: putChar :: Char -> IO ()

   将字符写入标准输出，\ :hs:func:`putStr`\ 函数即根据该函数定义。

   .. code-block::

      putStr' :: String -> IO ()
      putStr' [] = return ()
      putStr' (x : xs) = do
          putChar x
          putStr' xs

.. hs:function:: print :: Show a => a -> IO ()

   接受类型为\ ``Show``\ 类型类成员的值，对值调用\ ``show``\ 函数并写入标准输出，等价于\ ``putStrLn . show``\ ，是 GHCi 输出结果时默认调用的函数。

   .. code-block::

      main :: IO ()
      main = do
          print True      -- True
          print "Alice"   -- "Alice"
          print 2         -- 2
          print [2, 3, 4] -- [2,3,4]

输入函数
--------

.. hs:function:: getChar :: IO Char

   从键盘读取一个字符。

   .. code-block::

      main :: IO ()
      main = do
          c <- getChar
          if c /= ' '
              then do
                  putChar c
                  main
              else return ()

.. hs:module:: Control.Monad

控制函数
--------

.. hs:function:: when :: Applicative f => Bool -> f () -> f ()

   接受一个布尔值和 I/O 操作，若为\ ``True``\ 则返回该操作，否则返回\ ``return ()``\ ，适用于简化\ ``if``\ 语句。

   .. code-block::

      import Control.Monad

      -- | 上一个 'getChar' 函数的重写。
      main :: IO ()
      main = do
          c <- getChar
          when (c /= ' ') $ do
              putChar c
              main

.. hs:module:: Data.Traversable

.. hs:function:: sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)

   接受 I/O 操作列表并按顺序执行操作，返回值为已执行 I/O 操作返回结果的列表。

   .. code-block::

      main :: IO ()
      main = do                                      -- alice
          rs <- sequence [getLine, getLine, getLine] -- in
          print rs                                   -- wonderland
          -- ["alice","in","wonderland"]

.. hs:function:: mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)

   将函数对列表进行映射后，按顺序执行操作，等价于\ ``sequence (map f xs)``\ 。

   .. code-block::

      main :: IO ()
      main = mapM print [1..3]
           -- 1
           -- 2
           -- 3
           -- [(),(),()]

.. hs:module:: Data.Foldable

.. hs:function:: mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()

   与\ :hs:func:`~Data.Traversable.mapM`\ 函数相似，但丢弃返回结果。

   .. code-block::

      main :: IO ()
      main = mapM_ print [1..3]
           -- 1
           -- 2
           -- 3

.. hs:module:: Control.Monad

.. hs:function:: forever :: Applicative f => f a -> f b

   接受一个 I/O 操作并无限重复该操作。

   .. code-block::

      import Control.Monad
      import Data.Char

      main :: IO ()
      main = forever $ do
          putStr "Give me some input: "
          l <- getLine             -- Give me some input: alice
          putStrLn $ map toUpper l -- ALICE

.. hs:function:: forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)

   类似于\ :hs:func:`~Data.Traversable.mapM`\ 函数，但形参顺序相反。

   .. code-block::

      import Control.Monad

      main :: IO ()
      main = do
          colors <- forM
              [1..4]
              (\a -> do
                  putStrLn $ "Which color do you bind with " ++ show a
                  color <- getLine
                  return color
              )
          putStrLn "The colors you associate with 1, 2, 3, and 4 are:"
          mapM_ putStrLn colors

.. hs:function:: forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()

   与\ :hs:func:`forM`\ 函数类似，但丢弃返回结果。

随机
----

.. note::

   Haskell 中，相同参数调用相同函数，得到的一定是相同结果，为了获得（伪）随机数，需要\ :hs:mod:`System.Random`\ 模块。

   自 GHC 7.2.1 之后，GHC 便不再包含\ :hs:mod:`System.Random`\ 模块，因此需要自行下载。

.. hs:module:: System.Random

.. hs:function:: random :: (Random a, RandomGen g) => g -> (a, g)

   根据随机种子返回随机数和新的随机种子。

   ``Random``\ 类型类表示可成为随机值的数据，默认包含所有基本类型和\ ``Word``\ 类；

   ``RandomGen``\ 类型类表示可成为随机种子的数据。

   ``random*``\ 函数可使用类型注释改变随机值的类型。

   用相同的参数调用函数只能得到相同的结果，因此需要新的随机种子生成新的随机值。

.. hs:function:: mkStdGen :: Int -> StdGen

   根据整数生成随机种子。

   ``StdGen``\ 类型是\ ``RandomGen``\ 类型类的成员。

   .. code-block::

      import System.Random

      exp1 = mkStdGen 100
      -- StdGen {unStdGen = SMGen 16626... 25326...}
      exp2 = random $ mkStdGen 100
      -- (92164...,StdGen {unStdGen = SMGen 71263... 25326...})
      exp3 = fst (random (mkStdGen 100) :: (Bool, StdGen)) -- True
      exp4 = fst . random . mkStdGen $ 1 -- -2241774542048937483
      exp5 = fst . random . mkStdGen $ 1 -- -2241774542048937483

.. hs:function:: randoms :: (Random a, RandomGen g) => g -> [a]

   根据随机种子不断生成随机值。

   .. code-block::

      exp6 = take 5 $ randoms (mkStdGen 11) :: [Bool]
             -- [True,True,False,False,False]

.. hs:function:: randomR :: (Random a, RandomGen g) => (a, a) -> g -> (a, g)

   根据随机种子生成一个在序对规定范围内（闭区间）的随机值。

   .. code-block::

      exp7 = fst $ randomR (1,6) (mkStdGen 233333) -- 6

.. hs:function:: randomRs :: (Random a, RandomGen g) => (a, a) -> g -> [a]

   与\ :hs:func:`randomR`\ 类似，但生成无限个随机值。

   .. code-block::

      exp8 = take 3 $ randomRs (1, 6) (mkStdGen 233333) -- [6,2,3]
      exp9 = take 8 $ randomRs ('a', 'z') (mkStdGen 20) -- "itxegwun"

.. hs:function:: getStdGen :: MonadIO m => m StdGen

   获得全局随机种子。

   Haskell 启动程序时，会从系统处获得随机种子并储存在全局随机种子中，同一程序中全局随机种子不变。

   ``MonadIO``\ 类型类定义于模块\ :hs:mod:`Control.Monad.IO.Class`\ 。

   .. code-block::

      import System.Random

      main :: IO ()
      main = do
          gen <- getStdGen
          putStr $ take 20 (randomRs ('a', 'z') gen)

.. hs:function:: newStdGen :: MonadIO m => m StdGen

   将全局随机种子分为两份，用其中一份更新全局随机种子，返回另一个。

   .. code-block::

      import System.Random

      main :: IO ()
      main = do
          gen <- getStdGen
          putStr $ take 20 (randomRs ('a', 'z') gen)
          newgen <- newStdGen
          putStr $ take 20 (randomRs ('a', 'z') newgen)

.. code-block::

   module Main where

   import           Control.Monad                  ( when )
   import           System.Random                  ( Random(randomR)
                                                   , StdGen
                                                   , newStdGen
                                                   )

   main :: IO ()
   main = do
       newGen <- newStdGen
       let num = getNum newGen
       gameLoop num

   -- | 游戏循环。
   gameLoop :: Int -> IO ()
   gameLoop n = do
       putStrLn "Guess a number: "
       guess <- getLine
       let (code, msg) = putRes (read guess) n
       putStrLn msg
       when code (gameLoop n)

   -- | 生成随机数。
   getNum :: StdGen -> Int
   getNum = fst . randomR (1, 100)

   -- | 判断猜测是否正确。
   putRes :: Int -> Int -> (Bool, String)
   putRes g n | g > n     = (True, "Too big!")
              | g < n     = (True, "Too small!")
              | otherwise = (False, "Exactly!")
