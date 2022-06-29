.. highlight:: haskell

======
表达式
======

- 其他语言中的控制语句在 Haskell 中大多以表达式的形式存在；

``let``\ 表达式
===============

.. code-block::

   let <pattern> = <expression>
       ...
   in  <expression>

- ``let``\ 表达式：和\ ``where``\ :ref:`语句 <function:绑定>`\ 类似，\ ``let``\ 表达式可将表达式绑定至指定名字；
- 语法格式：

  - ``let``\ 关键字引导所有绑定；
  - 分行书写时一行书写一个绑定，单行书写时，多个绑定用分号\ ``;``\ 分隔；

    .. code-block::

       res = let a = 100; b = 200; c = 300 in a * b * c -- 6000000

  - ``let``\ 与\ :ref:`守卫语法 <function:守卫>`\ ：

    - ``let``\ 关键词下可使用守卫语法，绑定条件成立的值；

      .. code-block::

         let <variable>
             | <condition> = <result>
             ...
             | otherwise   = <result>
         in  <expression>

      - 当条件成立时，将结果与变量绑定；
      - 不成立则继续向下匹配；

    - 该语法与嵌套的\ ``if``\ :ref:`表达式 <expression:\`\`if\`\`\\ 表达式>`\ 效果相同；

      .. code-block::

         let a = if <condition>
               then <result>
               else if <condition>
                   then <result>
                   else <result>

  - ``in``\ 关键字定义作用域；

- 作用域：``let``\ 表达式的作用域只限于\ ``in``\ 关键字后的表达式；
- 绑定：

  - 同\ ``where``\ 语句，支持模式匹配和函数定义；

    .. code-block::

       res = let square x = x * x in (square 5, square 3, square 2)
             -- (25,9,4)

  - ``let``\ 表达式可用于\ :ref:`列表推导式 <tuple-and-list:列表推导式>`\ 中的输出部分，此时\ ``let``\ 表达式位于输入后、谓词前，且\ ``in``\ 关键字可省略；

    .. code-block::

       calcBmis :: (RealFloat a) => [(a, a)] -> [a]
       calcBmis xs =
           [ bmi
           | (w, h) <- xs
           , let bmi = w / h ^ 2
           , bmi >= 25.0
           ]

  - 当\ ``let``\ 表达式用于谓词中时，不能省略\ ``in``\ 关键字；

    .. code-block::

       res = [ x | x <- [1..10], let cond = even x in cond ]

- 区别：\ ``where``\ 语句为语法结构，是函数语法的一部分，只能单独使用，而\ ``let``\ 表达式为表达式，可用于任何表达式能用于的地方；

  .. code-block::

     res = 4 * (let a = 9 in a + 1) + 2 -- 42

``if``\ 表达式
==============

.. code-block::

   if <condition> then <expression1> else <expression2>

- ``if``\ 表达式：根据布尔值返回相应结果，若为\ ``True``\ 则返回\ ``expression1``\ ，否则返回\ ``expression2``\ ；
- ``if``\ 表达式与\ ``if``\ 语句不同，\ ``if``\ 语句中\ ``else``\ 可省略，而\ ``if``\ 表达式不可省略，因为必须有一个最终值；
- Haskell 中\ ``if``\ 表达式不常用，通常使用\ :ref:`守卫语法 <function:守卫>`\ ；

多路\ ``if``\ 表达式
====================

.. code-block::

   {-# LANGUAGE MultiWayIf #-}
   if | <condition> -> <result>
      ...

- :tr:`多路 (multiway)`\ ``if``\ 表达式：\ ``if``\ 表达式的\ :ref:`扩展语法 <unfinished>`，允许\ ``if``\ 表达式进行多个条件的判断；
- 语法格式：

  - \ ``if``\ 引领所有条件判断；
  - \ ``|``\ 后定义条件；
  - ``->``\ 后定义条件为真时的解析结果；

- 使用多路\ ``if``\ 表达式需要手动开启语法支持，添加\ ``{-# LANGUAGE MultiWayIf #-}``\ 于文件开头第一行即可；

.. code-block::

   {-# LANGUAGE MultiWayIf #-}
   describeList :: [a] -> String
   describeList xs = "The list is " ++ if
       | null xs        -> "empty."
       | length xs == 1 -> "a singleton list."
       | otherwise      -> "a longer list."

``case``\ 表达式
================

.. code-block::

   case <expression> of <pattern> -> <result>
                        ...

- ``case``\ 表达式：对表达式进行模式匹配，并解析为相应结果；
- 函数参数的模式匹配即为\ ``case``\ 表达式的语法糖；
- 语法格式：

  - ``case``\ 关键字引领模式匹配；
  - ``case``\ 关键字后跟要进行匹配的表达式；
  - ``of``\ 关键字定义模式匹配，后跟多个模式，表达式依次与模式进行匹配；
  - ``->``\ 后定义匹配成功后的解析结果；

- 与函数参数的模式匹配不同，\ ``case``\ 表达式可以用于任何表达式能用于的地方；

.. code-block::

   describeList :: [a] -> String
   describeList xs = "The list is " ++ case xs of
       []  -> "empty."
       [x] -> "a singleton list."
       xs  -> "a longer list."
