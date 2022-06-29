.. highlight:: haskell

========
高阶函数
========

- :tr:`高阶函数 (higher order function)`\ ：可接受函数作为参数、或可返回函数作为返回值的函数；

柯里化
======

- 源于 :ref:`λ 演算 <lambda-calculus:柯里化>`\ ；
- Haskell 中的函数严格来说只接受一个参数，通过柯里化可以实现多个参数的接受；

  .. note::

     ``Int -> Int -> Int``\ 实际表达的意思为\ ``Int -> (Int -> Int)``\ ，即接受一个整数，返回一个函数，该函数接受并返回一个整数。

     因为\ ``->``\ 为右结合，所以\ ``Int -> (Int -> Int)``\ 等价于\ ``Int -> Int -> Int``\ 。

部分应用
========

- :tr:`部分应用 (partial application)`\ ：因为柯里化，当参数接受的实参少于形参时，函数会返回一个包含剩余形参的函数，这种用法称为部分应用；

  .. code-block::

     partialAdd :: Num a => a -> a
     partialAdd = (+ 3)  -- (+) :: Num a => a -> a -> a

     exp1 = partialAdd 5 -- 8

- 部分应用函数可以在执行函数时创造函数；

  .. code-block::

     exp2 = filter (> 3) [1, 5, 3, 2, 1, 6, 4, 3, 2, 1] -- [5,6,4]
     exp3 = filter (`elem` ['A'..'Z']) "aLice In woNdErlanD" -- "LINED"
     exp4 = let listOfFuncs = map (*) [0 ..] in (listOfFuncs !! 4) 5 -- 20

.. attention::

   ``-``\ 运算符是 Haskell 中唯一既可做前缀又可做中缀的运算符，因此\ ``(- 3)``\ 会解释为负三，而非部分应用。若要部分应用减法，应使用\ :hs:func:`subtract`\ 函数。

Lambda 函数
===========

.. code-block::

   \<var> ... -> <functionBody>

- Lambda 函数：匿名函数；
- 语法格式：

  - ``\``\ ：定义一个 Lambda 函数；

    .. note::

       ``\``\ 实际上是 λ 的简写。

  - ``x``\ ：参数，多个参数之间用空格分隔；

    .. note::

       ``\x y z -> ...``\ 实际上等价于\ ``\x -> \y -> \z -> ...``\ 。

  - ``->``\ ：定义函数体，默认向右无限扩展；

- Lambda 函数支持模式匹配，但仅支持单个匹配，若不匹配则报错；

.. code-block::

   exp1 = map (\x -> 7 * x + 2) [1 .. 5]  -- [9,16,23,30,37]
   exp2 = zipWith (\a b -> (a * 30 + 3) / b) [1 .. 3] [4 .. 6]
                                          -- [8.25,12.6,15.5]
   exp3 = map (\(a, b) -> a + b) [(1, 2), (3, 5), (6, 3), (2, 6)]
                                          -- [3,8,9,8]

.. hs:module:: GHC.Base
   :synopsis: 基础数据类型及类型类。

函数组合
========

.. hs:function:: (.) :: (b -> c) -> (a -> b) -> a -> c

   .. code-block::

      <function1> . <function2> . ...

   - :tr:`函数组合 (function composition)`\ ，即将多个函数链式组合，使得上一个函数的结果作为下一个函数的输入值。

   - 在数学上函数组合定义为 :math:`(f\circ g)(x)=f(g(x))`，即 :math:`g(x)` 的结果作为 :math:`f(x)` 的自变量继续进行计算。

   - 类型：第一个函数接受的参数类型必须与第二个函数的返回值类型相同，最终的返回值类型与第一个函数的返回值类型相同。

   - 结合性：函数组合\ :hs:func:`. <GHC.Base.(.)>`\ 默认为右结合，因此首先对第二个函数进行解析。

     .. code-block::

        exp1 = map (\x -> negate (abs x)) [5, -3, 7] -- [-5,-3,-7]
        exp2 = map (negate . abs) [5, -3, 7]         -- [-5,-3,-7]

   - 若有多参数函数，则必须先对函数进行部分应用，再组合函数。

     .. code-block::

        exp3 = (sum . takeWhile (< 10000) . filter odd . map (^2)) [1 ..]

   - :tr:`无值编程 (pointfree style)`\ ：当函数最右端为参数时，可省略该形参，返回一个函数；

     .. code-block::

        sum' :: (Foldable t, Num b) => t b -> b
        sum' xs = foldl (+) 0 xs  -- "xs" here is unneccessary

        sum' :: GHC.Types.Any Integer -> Integer
        sum' = foldl (+) 0        -- Returns a function that accepts lists

        fn :: (RealFrac a, Integral b, Floating a) => a -> b
        fn x = ceiling (negate (tan (cos (max 50 x))))

        fn :: Double -> Integer
        fn = ceiling . negate . tan . cos . max 50
                                  -- Returns a function

   - 函数组合有时可以提高代码可读性，利于无值编程，但并不是任何时候函数组合都能提高可读性；

     .. code-block::

        addSqSum :: Integer
        addSqSum = (sum . takeWhile (<10000) . filter odd . map (^ 2)) [1 ..]

        addSquareSum :: Integer -- Maybe more readable
        addSquareSum =
            let oddSquares = (filter odd . map (^ 2)) [1 ..]
                belowLimit = takeWhile (< 10000) oddSquares
            in  sum belowLimit

函数应用\ ``$``
===============

.. hs:function:: ($) :: (a -> b) -> a -> b

   - 与空格相同，应用函数，将第二个表达式的结果传入第一个函数；
   - 区别：

     - 空格优先级最高，\ :hs:func:`$ <GHC.Base.($)>`\ **优先级最低**\ ；

       .. code-block::

          exp1 = sqrt 3 + 4 + 9   -- 10.732050807568877
          exp2 = sqrt $ 3 + 4 + 9 -- 3.4641016151377544

     - 空格左结合，\ :hs:func:`$ <GHC.Base.($)>`\ **右结合**\ ；

       .. code-block::

          exp3 = sum (map sqrt [1 .. 130]) -- (map ...) 先求值
          exp4 = sum $ map sqrt [1 .. 130] -- 等价

   - 作用：

     - 与括号相同，调整函数应用的优先级，但\ :hs:func:`$ <GHC.Base.($)>`\ 可以帮助省略括号，提高可读性；

       .. code-block::

          exp5 = sum . takeWhile (< 10000) . filter odd . map (^ 2) $ [1 ..]

     - 可将函数应用的过程转化为一个函数；

       .. code-block::

          exp6 = map ($ 3) [(4 +), (10 *), (^ 2)] -- [7,30,9]
