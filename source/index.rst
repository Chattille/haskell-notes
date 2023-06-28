.. toctree::
   :hidden:

   lambda-calculus
   type
   operator
   function
   stack
   tuple-and-list
   expression
   recursion
   high-order-function
   module
   map
   set
   algebraic-data-type
   haddock
   type-class
   io
   file
   error
   functor-monoid
   monad
   exercise

.. image:: _images/haskell.svg
   :alt: Haskell
   :align: center

.. hlist::
   :columns: 3

   - :ref:`genindex`
   - :ref:`hs-modindex`

.. role:: strike
   :class: strike

============
Haskell 简介
============

简介
====

- Haskell_ 是一种标准化的纯函数式、强静态类型编程语言；

特点
----

- :tr:`纯函数式 (purely functional)`\ ：

  - **函数是第一类对象**\ ，函数可像整数等其他数据类型一样使用，并作为主要控制结构使用；
  - Haskell 程序不以\ :tr:`执行指令 (executing instructions)`\ 为中心，而以\ :tr:`求表达式的值 (evaluating expressions)`\ 为中心；

  .. note::

     命令式语言通过指令命令计算机“做什么”（将 1 到某数之间的所有数字相乘）。

     而函数式语言通过函数描述问题“是什么”（“阶乘是从1到某数间所有数字的乘积”）。

- :tr:`引用透明 (referentially transparent)`\ ：

  - 无可变性，一切数据都\ :tr:`不可变 (immutable)`\ ；
  - 表达式没有副作用（如更改全局变量或输出变量）；
  - 调用相同函数并传入相同参数，得到的\ **结果始终相同**\ ；

- :tr:`惰性 (lazy)`\ ：表达式在不需要时不会求值；

  .. note::

     设有列表\ ``xs = [1,2,3,4,5,6,7,8]``\ 和可以将列表所有元素都乘以二的函数\ ``doubleMe``\ 。在命令式语言中，\ ``doubleMe(doubleMe(doubleMe(xs)))``\ 需要将列表遍历三遍才能得到结果；而在惰性语言中，最里层的函数会先将列表第一个元素乘以二，然后传递给中层函数，最后传递给最外层函数，得到结果 8，之后再提取第二个元素，依此类推。

- :tr:`静态类型 (statically typed)`\ ：编译时需要明确表达式的类型；
- :tr:`类型推断 (type inference)`\ ：Haskell 能自动推导类型，不需要声明所有变量；

优点
----

- 引用透明表示可通过等式进行公式推导；
- 可通过定义新函数来创造新的控制结构；
- 可定义和处理无限长度的数据结构；
- 实现一种\ :tr:`可分解 (compositional)`\ 的编程风格；

缺点
----

- 时间和空间复杂度更难计算；

简史
====

- 20 世纪 30 年代，美国数学家\ :tr:`阿隆佐·邱奇 (Alonzo Church)`\ 首次发表 :doc:`λ 演算 <lambda-calculus>`\ ，通过符号表达变量的绑定和替换来抽象化定义函数计算，用于表示任何图灵机，是函数式语言的重要基石；
- 1958 年，斯坦福大学教授\ :tr:`约翰·麦卡锡 (John McCarthy)`\ 受卡内基大学开发的 :abbr:`IPL (Information Processing Language)` 语言影响，开发出函数式语言 :abbr:`Lisp (List Processing)`\ ；
- 20 世纪 60 年代，牛津大学的\ :tr:`彼得·兰丁 (Peter Landin)`\ 和\ :tr:`克里斯托夫·斯特拉奇 (Christopher Strachey)`\ 明确了 λ 演算对函数式编程具有极高的重要性；
- 1966 年， 两人开发出基于 λ 演算的纯函数式语言 :abbr:`ISWIM (If you See What I Mean)`\ ，奠定了函数式语言设计的基础；
- 1975 年， 麻省理工学院的\ :tr:`格里·萨斯曼 (Gerry Sussman)`\ 和\ :tr:`盖伊·斯蒂尔 (Guy Steele)`\ 开发出更接近 λ 演算的语言 Scheme，广泛应用于实践和教学中；
- 1977 年，美国计算机科学家\ :tr:`约翰·巴克斯 (John Backus)`\ 开发出函数式语言 FP，引入了 BNF 符号系统，开发了语言编译器，并获得图灵奖；
- 同时期，剑桥大学的\ :tr:`罗宾·米尔纳 (Robin Milner)`\ 开发出 :abbr:`ML (Meta-Language)`\ ，并发展出多态类型系统 :abbr:`HM-System (Hindley-Milner type system)`\ ，包括类型推断和类型安全与异常处理机制，Haskell 使用的便是该种类型系统；
- 1985 年，Miranda 发行，惰性函数式语言的关注度增长；
- 1987 年之前，出现了十多种纯函数式语言，其中 Miranda 使用最广泛；
- 1987 年，俄勒冈波特兰的函数式编程语言与计算机结构大会上，参会者决定成立委员会集成已有函数式语言，为这种语言定义一种开放标准，该语言将用作研究和教授函数式编程的工具，委员会用美国数学家\ :tr:`哈斯克尔·布鲁克斯·柯里 (Haskell Brooks Curry)`\ 的名字来命名该语言，以纪念他在 λ 演算和组合逻辑方面作出的贡献；
- 1990 年，委员会发布 Haskell 1.0，并在之后形成了一系列语言定义；
- 1997 年，委员会发布 Haskell 98；
- 1999 年， Haskell 98 标准公布，名为\ :title-reference:`《The Haskell 98 Report》`\ ；
- 2003 年，\ :title-reference:`《Haskell 98 Language and Libraries: The Revised Report》`\ 公布；
- 同年，Glasgow Haskell Compiler 实现该标准；
- 2006 年， Haskell 98 标准继续改进，非正式版命名为 Haskell Prime，每年产生一个修订版；
- 2010 年，第一个修订版发布，命名为 `Haskell 2010`_\ ，其影响延续至今；


工具
====

Haskell 项目
------------

- |GHC|_：Haskell 编译器；
- :abbr:`GHCi (Glasgow Haskell Compiler interactive)`\ ：Haskell 的交互式解释器，GHC 自带；
- Cabal_\ ：Haskell 包管理器和包生成器；
- GHCup_：GHC 和 Cabal 的工具链安装器；
- Stack_：跨平台的 Haskell 项目管理工具，包括 GHC 安装、项目编译、依赖安装等；
- `Haskell Platform`_\ ：\ :strike:`打包的 Haskell 开发环境，包括以上所有工具，并附加 35 个核心库和其他广泛使用的包，支持代码覆盖率检查`\ ；

  .. note::

     `Haskell Platform`_ 2022年后已弃用，推荐使用 GHCup_\ 。

- |HLS|_\ ：Haskell 语言服务器，可集成于编辑器；
- Haddock_\ ：事实上的 Haskell 文档生成器；

.. note::

   推荐使用 GHCup_ 安装 GHC、Cabal、HLS 和 Stack，再\ :doc:`使用 Stack 管理项目 <stack>`\ 。

IDE
---

- VSCode：Haskell 插件；
- IntelliJ IDEA：HaskForce 插件；

文件
====

- ``.hs``\ ：Haskell 源代码文件；

  - 单行注释：连续的两个减号之后的所有内容都会视为注释；

    .. code-block:: haskell

       -- 单行注释

  - 多行注释：大括号/减号对内为注释；

    .. code-block:: haskell

       {- 多行注释用
          减号和花括号包围 -}

- ``.lhs``\ ：Literate Haskell 文件，支持文学编程；

  - :tr:`文学编程 (literate programming)`\ ：由\ :tr:`唐纳德·克努特博士 (Dr. Donald Knuth)`\ 发明，旨在让代码成为人人间的交流工具而非人机间的指令集合；
  - 可用 Haddock 工具将\ ``.lhs``\ 文件转换为 Haskell 文档；
  - 格式：

    - Bird 风格：由\ :tr:`理查德·伯德 (Richard Bird)`\ 发明；

      .. code-block:: literate-haskell

         该文件为 Literate Haskell 文件。
         任何字符均视为注释。

         > x :: Int  -- 这里是代码块
         > x = 3

         代码块前后必须有空行。

      - 任何以\ ``>``\ 后跟一个空格开头的行为代码行，前后必须各保留一个空行；
      - 其余内容均视作注释；

    - LaTeX 风格：

      .. code-block:: literate-haskell

         该文件为 LaTeX 风格的文学 Haskell 文件。
         \begin{code}
         y :: [Integer]
         y = [1,2,3,4]
         \end{code}

      - 代码块使用\ ``\begin{code}``\ 和\ ``\end{code}``\ 环境包围；
      - 可使用\ ``lhs2TeX``\ 工具将\ ``.lhs``\ 转换为 Haskell 文档；

.. _Haskell: https://www.haskell.org/
.. _`Haskell 2010`: https://www.haskell.org/onlinereport/haskell2010/
.. _GHC: https://www.haskell.org/ghc/
.. _Cabal: https://www.haskell.org/cabal/
.. _GHCup: https://www.haskell.org/ghcup/
.. _Stack: https://docs.haskellstack.org/en/stable/README/
.. _`Haskell Platform`: https://www.haskell.org/platform/
.. _HLS: https://github.com/haskell/haskell-language-server/
.. _Haddock: https://www.haskell.org/haddock/

.. |GHC| replace:: :abbr:`GHC (Glasgow Haskell Compiler)`
.. |HLS| replace:: :abbr:`HLS (Haskell Language Server)`
