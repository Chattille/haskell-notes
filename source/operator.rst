.. highlight:: haskell
   :linenothreshold: 10

======
运算符
======

总览
====

- Haskell 中，所有运算符本质上均为函数，在 GHCi 中使用\ ``:type``\ 命令检查运算符类型时，须用小括号\ ``()``\ 包围；

  .. code-block::

     (+) :: Num a => a -> a -> a

- Haskell 中，所有运算符都默认为\ :tr:`中缀运算符 (infix operator)`\ ，即参数分布在左右两侧，小括号\ ``()``\ 可以将运算符转换为\ :tr:`前缀函数 (prefix function)`\ ；

  .. code-block::

     res1 = 2 + 3   -- 5
     res2 = (+) 2 3 -- 5

- Haskell 中，使用一对反引号可将接受两个参数的函数转换为中缀运算符，此时左运算子作为第一个位置参数传入；

  .. code-block::

     addFunc :: Integer -> Integer -> Integer -- 函数定义
     addFunc x y = x + y

     res1 = addFunc 1 2
     res2 = 1 `addFunc` 2
     res1 == res2 -- True

算术运算符
==========

======== ==================
运算符   说明
======== ==================
``+``    加
``-``    减
``*``    乘
``/``    除（浮点）
``quot`` 除（向零取整）
``div``  除（向负取整）
``mod``  取模
``rem``  取余
``^``    幂（整幂）
``^^``   幂（浮点底整幂）
``**``   幂（浮点底浮点幂）
======== ==================

- ``/``\ ：两侧只接受浮点数为参数，否则报错；

  .. code-block::

     i :: Integer
     i = 3
     x = i / i
     -- The type signature for ‘i’ lacks an accompanying binding

- ``div``\ 和\ ``mod``\ ：Haskell 中整除和取模没有对应运算符，但有对应函数；

  .. code-block::

     x = (-7) `mod` 3 -- 2
     y = 3 `div` 2    -- 1

- ``^``\ 右侧只接受整数，\ ``^^``\ 左侧接受浮点数，右侧接受整数，\ ``**``\ 两侧只接受浮点数；
- Haskell 中，任何时候负数都必须用括号包围，否则会识别为减号；

  .. code-block::

     z = (-3) * (-7)  -- returns 21
     z = -3 * -7
     -- Precedence parsing error
     --     cannot mix ‘*’ [infixl 7] and prefix `-' [infixl 6]
     --         in the same infix expression

- 所有运算符两侧（除幂运算）必须为\ **同一数值数据类型**\ ，Haskell 不作隐式类型转换；

  .. code-block::

     j :: Float
     k :: Double
     j = 3.2
     k = 3.2
     l = j + k
     -- Couldn't match expected type ‘Float’ with actual type ‘Double’

逻辑运算符
==========

======= ======
运算符  说明
======= ======
``&&``  逻辑与
``||``  逻辑或
``not`` 逻辑非
======= ======

.. _predicate:

- :tr:`谓词 (predicate)`\ ：

  - 可进行真值判断的表达式，且该表达式决定是否执行后续操作；

    .. code-block::

       res1 = if "Haskell" > "C++" -- 谓词
           then 3
           else 4

  - 在函数参数的类型声明中，若该函数第一个参数为一个返回布尔值的函数，则该函数也称为谓词；

.. code-block::

   res1 = True && False
   res2 = not (False || True)

比较运算符
==========

====== ==========
运算符 说明
====== ==========
``==`` 等于
``/=`` 不等于
``>``  大于
``<``  小于
``<=`` 大于或等于
``>=`` 小于或等于
====== ==========

- 与其他语言不同，Haskell 使用\ ``/=``\ 作为不等于；
- 当比较运算符两侧为字符时，比较字符的 Unicode 码，若为字符串，则从左向右依次比较；
- 当比较运算符两侧为\ :doc:`列表 <tuple-and-list>`\ 时，从左至右依次对每个元素进行比较（若均相对，则元素更多者更大）；

  .. code-block::

     res = [3,4,2] > [3,4] -- True

固定性声明
==========

- :tr:`固定性 (fixity)`\ ：即运算符的优先级，在 Haskell 中分为 0~9 共 10 个等级，数字越大运算优先级越高；
- 结合性：Haskell 中分为左结合、右结合和无结合三种；

  - 左结合：首先与左运算子运算；
  - 右结合：首先与右运算子运算；
  - 无结合：不结合；

- ``infix*``\ 关键字：

  .. code-block::

     infix <fixity> <operator>
     infixl <fixity> <operator>
     infixr <fixity> <operator>

  - ``infix``\ ：指定无结合运算符的固定性；
  - ``infixl``\ ：指定左结合运算符的固定性；
  - ``infixr``\ ：指定右结合运算符的固定性；
  - ``infix*``\ 关键字同样可以定义中缀函数的固定性；

- 运算符命名：除少数保留字外，Haskell 中的运算符可以用任意非标识符字符定义；

  - 合法字符：\ ``!#$%&*+./=<?>@\~ˆ-:``\ ；
  - 保留字：\ ``..``\ 、\ ``:``\ 、\ ``::``\ 、\ ``=``\ 、\ ``\``\ 、\ ``|``\ 、\ ``<-``\ 、\ ``->``\ 、\ ``@``\ 、\ ``~``\ 和\ ``=>``\ ；

.. code-block::

   infixl 6 -?+
   (-?+) :: (Num a, Ord a) => a -> a -> a
   x -?+ y | x > y = x - y
           | otherwise = x + y

   exp1 = 3 -?+ 2       -- 1
   exp2 = 3 -?+ 4       -- 7
   exp3 = 3 -?+ 2 -?+ 5 -- 6

内置运算符
==========

.. list-table::
   :header-rows: 1
   :widths: 1 4 5 2

   * - 优先级
     - 左结合
     - 无结合
     - 右结合
   * - 9
     - ``!!``
     -
     - ``.``
   * - 8
     -
     -
     - ``^``, ``^^``, ``**``
   * - 7
     - ``*``, ``/``, ``div``, ``mod``, ``rem``, ``quot``
     -
     -
   * - 6
     - ``+``, ``-``
     -
     -
   * - 5
     -
     -
     - ``:``, ``++``
   * - 4
     -
     - ``==``, ``/=``, ``<``, ``<=``, ``>``, ``>=``, ``elem``, ``notElem``
     -
   * - 3
     -
     -
     - ``&&``
   * - 2
     -
     -
     - ``||``
   * - 1
     - ``>>``, ``>>=``
     -
     -
   * - 0
     -
     -
     - ``$``, ``$!``, ``seq``
