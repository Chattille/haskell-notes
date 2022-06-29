.. highlight:: haskell
   :linenothreshold: 10

====
函数
====

函数定义
========

.. code-block::

   <functionName> :: <paramType> -> ... -> <returnType>

- 参数类型：

  - ``::``\ 之后定义函数的参数类型，每个参数都必须定义类型；
  - 多个参数类型用\ ``->``\ 顺序链接，\ ``->``\ 默认为右结合；

    .. note::

       在 Haskell 中，每个函数严格意义上都是\ **单参数函数**\ ，通过\ :ref:`柯里化 <lambda-calculus:柯里化>`\ 实现多参数函数；

       使用\ ``->``\ 而不用其他符号（如逗号）分隔的原因也是\ :ref:`柯里化 <high-order-function:柯里化>`\ 。

  - 函数：当函数作为参数传入时，该函数参数类型声明格式不变，但整体用小括号\ ``()``\ 包围；

    .. code-block::

       -- all :: Foldable t => (a -> Bool) -> t a -> Bool
       --                      ^~~~~~~~~~^: 函数作参数
       -- odd :: Integral a => a -> Bool
       res2 = all odd [1,2,3,4] -- False

- 返回值类型：最后一个类型用于定义函数返回值的类型；

.. code-block::

   factorial :: Integer -> Integer
   -- 该函数名为 'factorial'，接受一个 Integer 类型 作为参数，
   -- 返回一个 Integer 类型

函数应用
========

.. code-block::

   <functionName> <parameterList>

- 应用：并列函数和参数列表，用空格分隔；

  .. code-block::

     factorial 4 -- 求值为 4!

- 优先级：函数应用的优先级高于中缀运算符；

  .. code-block::

     func 3 n+1 7 -- 同 (func 3 n)+(1 7)
     func 3 (n+1) 7

模式匹配
========

.. code-block::

   <functionName> :: <parameterType> -> ... -> <returnType>
   <functionName> <pattern> = <functionBody>
   ...

- :tr:`模式 (pattern)`\ ：

  - 函数参数的可能形式；
  - 模式可以是任何数据类型的字面量或标识符；
  - 字面量表示其本身，而标识符匹配任何数据类型；

    .. code-block::

       hailstone 1 = 1     -- 匹配数字 1
       hailstone n = n + 1 -- 匹配任意值

  - 模式中可以使用多个变量或字面量指定接受多个参数；

    .. code-block::

       foo x y z = x + y + z -- 接受三个参数

  - 若函数体中不使用参数，则多余的参数可用\ ``_``\ 代替；

    .. code-block::

       first :: (a, b, c) -> a
       first (x, _, _) = x -- 匹配数对中的第一个元素

- 语法格式：

  - 第一行为函数类型声明，声明函数参数与返回值的类型；
  - 类型声明之下为模式匹配，每行以函数名开始，表示所有模式都属于该函数，各函数名之间可空行，但必须为同一函数名，否则认为函数定义结束；
  - 每行函数名后跟模式，表示函数参数的可能形态；
  - 用\ ``=``\ 分隔模式和函数体；
  - ``=``\ 后跟函数体；
  - 模式数量不限；
  - 当接受两个参数时，函数名部分可以使用反引号包围转换为中缀运算符；

    .. code-block::

       myAdd :: (Num a) => a -> a -> a
       a `myAdd` b = a + b -- 等价于 myAdd a b = a + b

- 工作流程：

  1. 检查参数值是否符合参数类型，不符合则报错（\ ``expected type``\ ），符合则继续；
  2. 在 Haskell 中，函数体\ **自上向下**\ 逐一检查参数是否与模式相匹配；
  
     - 若参数不匹配模式，则继续向下匹配；
     - 若参数匹配模式，则解析模式对应的函数体；
  
  3. 若有匹配结果，则解析函数体，检查返回值是否符合返回值类型，不符合则报错（\ ``expected type``\ ），符合则结束函数；
  4. 若无匹配结果，则报错（\ ``non-exhaustive patterns``\ ）；

     .. note::

        为确保模式匹配的全面性，建议在最后将标识符作为模式，以匹配任何参数。

.. code-block::
   :linenos:

   factorial :: Integer -> Integer -- 函数定义
   factorial 1 = 1 -- 第一个模式匹配，匹配整数 1
   factorial x = x * factorial (x - 1)
             -- 第二个模式匹配，标识符 'x' 匹配任意值

   exp1 = factorial 1
   -- 匹配第一个模式，求值为 1
   -- exp1 == 1
   exp2 = factorial 3
   -- 不匹配第一个模式，但匹配标识符 'x'，函数求值为 3 * factorial (3 - 1)
   -- exp2 == 6
   exp3 = factorial 0
   -- 匹配第二个模式 'x'，求值为 0 * factorial (0 - 1)
   -- 死循环

守卫
====

.. code-block::

   <functionName> <pattern>
       | <condition> = <functionBody>
       ...
       | otherwise   = <functionBody>

- :tr:`守卫 (guard)`\ ：用于检查表达式的\ **真值**\ ；
- 语法格式：

  - 守卫语法始于模式之后；
  - ``|``\ 表示守卫的开始，通常缩进两个空格；
  - ``|``\ 后接条件，用于判断真值；
  - ``=``\ 后定义函数体；
  - 守卫可以有多个，多个守卫之间可以有空行，但必须在同一模式下，否则认为守卫定义结束；
  - ``otherwise``\ 关键字：布尔值\ ``True``\ 的别名，用于兜底；
  - 守卫也可以书写在同一行，但为了可读性，不建议这样书写；

    .. code-block::

       max :: (Ord a) => a -> a -> a
       max a b | a > b = a | otherwise = b

- 工作流程：

  1. 参数类型检查；
  2. 匹配模式，若匹配成功则进入守卫，否则继续向下匹配；

     1. 从上至下依次检查表达式的真值，若为真，则解析函数体；
     2. 若为假，则继续向下检查；
     3. 若均为假，则\ **继续匹配下一个模式**\ ；

  3. 后同\ :ref:`模式匹配 <function:模式匹配>`\ ；

.. code-block::
   :linenos:

   foo :: Integer -> Integer
   foo 0 = 16
   foo 1 | "Haskell" > "C++" = 3
         | otherwise         = 4
   foo n | n < 0           = 0
         | n `mod` 17 == 2 = -43
         | otherwise       = n + 3

   exp1 = foo 0 -- 匹配第一个模式，求值为 16
   exp2 = foo 36
        -- 匹配最后一个模式，对守卫进行求值：
           -- 36 < 0 判断为假，继续
           -- 36 `mod` 17 == 2 判断为真，最终求值为 -43
   exp3 = foo 38
        -- 匹配最后一个模式，对守卫进行求值：
           -- 38 < 0 判断为假，继续
           -- 38 `mod` 17 == 2 判断为假，继续
           -- 最终求值为 38 + 3

绑定
====

.. code-block::

   where <pattern> = <expression>
         ...

- ``where``\ 语句：将表达式绑定至指定名字，执行时，名字会替换为相应表达式；
- ``where``\ 语句用于提高代码的重用率，避免相同代码重复出现；
- 语法格式：

  - ``where``\ 语句可定义多个绑定，一个\ ``where``\ 关键字可引领多个绑定；
  - ``=``\ 前定义被绑定的名字，\ ``=``\ 后定义绑定并要替换的表达式；
  - 一行书写一个绑定；

- 作用域：

  - 当\ ``where``\ 语句位于函数之后时，作用域为当前函数；

    .. code-block::

       bmiTell :: (RealFloat a) => a -> a -> String
       bmiTell weight height | bmi <= skinny = "You're underweight."
                             | bmi <= normal = "You're supposedly normal."
                             | bmi <= fat    = "You're overweight."
                             | otherwise     = "You're a whale!"
         where
           bmi                   = weight / height ^ 2
           (skinny, normal, fat) = (18.5, 25.0, 30.0)
       -- 求值时，'bmi'，'skinny' 和其他标识符会替换为 'where' 语句中 '=' 后的部分

  - 当\ ``where``\ 语句位于所有代码之前时，作用域为全局；

- 替换：

  - 模式匹配：和函数模式匹配一样，\ ``where``\ 语句也支持模式匹配；

    .. code-block::
       :linenos:

       describeList :: [a] -> String
       describeList xs = "The list is " ++ what xs
         where
           what []  = "empty."
           what [x] = "a singleton list."
           what xs  = "a longer list."

       res = describeList [2] -- "The list is a singleton list."

  - 函数：\ ``where``\ 语句中可以定义并替换函数；

    .. code-block::

       calcBmis :: (RealFloat a) => [(a, a)] -> [a]
       calcBmis xs = [ bmi w h | (w, h) <- xs ]
           where bmi weight height = weight / height ^ 2
