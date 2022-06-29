.. highlight:: haskell
   :linenothreshold: 10

=======
Haddock
=======

简介
====

- Haddock：根据 Haskell 源码和注释生成文档的工具，是 Haskell 事实上的文档生成器；
- Haddock 使用特殊注释生成文档，没有特殊注释 Haddock 也可从签名和类型声明生成文档；
- Haddock 支持 HTML、LaTeX 格式和 hoogle 后端；
- 下载：\ 官网_\ ，通常已整合到 GHC 中；
- 使用：

  .. code-block:: console

     $ haddock [OPTION...] file...

  .. rubric:: 参数

  生成文档的目标文件，接受\ :file:`.hs`\ 和\ :file:`.lhs`\ 文件。

  .. rubric:: 选项

  .. program:: haddock

  .. option:: --odir <dir>, -o <dir>

     生成文档于指定目录，默认为当前目录。

  .. option:: --html, -h

     生成 HTML 格式的文档。目录结构：

     - :file:`{MODULE-NAME}.html`\ ：各模块的文档；
     - :file:`mini_module.html`\ ：各模块梗概文档；
     - :file:`index.html`\ ：顶级页面；
     - :file:`doc-index-{X}.html`\ ：按字母表排序的文档；
     - :file:`{SOME}.css`\ ：使用到的 CSS；
     - :file:`haddock-util.js`\ ：使用到的 JavaScript 文件；

  .. option:: --latex

     生成 LaTeX 格式的文档。目录结构：

     - :file:`pacakge.tex`\ ：顶级 LaTeX 源文件；
     - :file:`haddock.sty`\ ：默认样式；
     - :file:`{MODULE}.tex`\ ：各模块的文档；

  .. option:: --hoogle

     生成用于 Hoogle 引擎后端的索引文件，必须设置\ :option:`--package-name <haddock --package-name>`\ 选项。

  .. option:: --mathjax <url>

     指定 MathJax 地址。

  .. option:: --theme <path>

     指定为\ :option:`--html <haddock --html>`\ 选项使用的样式表地址。

  .. option:: --pretty-html

     生成 HTML 文件时使用换行和缩进。

  .. option:: --use-unicode

     生成 HTML 文件时使用 Unicode 字符集。

  .. option:: --latex-style <style>

     指定为\ :option:`--latex <haddock --latex>`\ 选项使用的样式文件。

  .. option:: --package-name <name>

     设置包名。

  .. option:: --package-version <version>

     设置包版本。

  .. option:: --help, -?

     帮助信息。

语法
====

顶级声明
--------

.. code-block::

   -- |<annotation>
   -- ...
   <top-level declaration>

- 顶级声明：函数类型签名、类型声明、\ ``class``\ 声明、\ ``data``\ 声明、\ ``pattern``\ 声明、\ ``newtype``\ 声明、\ ``type``\ 声明、\ ``instance``\ 声明等；
- ``-- |``：

  - 在顶级声明的上一行添加该特殊注释，对该声明进行文档注释；
  - 也可将注释置于声明的下一行，类似于 Python 的 Docstring；
  - 支持多行文档注释，文档注释持续到第一个非注释行；

  .. code-block::

     -- | 函数 'square' 对一个整数求平方根。
     -- 接受一个 'Int' 类型的参数。
     square :: Int -> Int -- 顶级函数声明
     square x = x * x
  
- ``{-|<annotation>-}``\ ：多行注释也支持文档注释；

  .. code-block::

     {-|
       函数 'square' 对一个整数求平方根。
       接受一个 'Int' 类型的参数。
     -}
     square :: Int -> Int
     square x = x * x

部分声明
--------

.. code-block::

   -- ^<annotation>

- 部分声明：对声明中的一部分进行文档注释；
- ``-- ^``\ ：用于一行末尾，对当前行起作用；

  - 当值构造器和其参数位于同一行时，文档注释作用于值构造器；
  - ``data``\ 声明后的\ ``deriving``\ 关键字只能使用\ ``-- ^``\ ；

  .. code-block::

     data T a b
       = C1     -- ^ 'C1' 构造器说明
           a    -- ^ 'C1' 参数 'a' 说明
           b    -- ^ 'C1' 参数 'b' 说明
       | C2 a b -- ^ 'C2' 构造器说明
       deriving ( Eq  -- ^ 类型类 'Eq' 说明
                , Ord -- ^ 类型类 'Ord' 说明
                )

     data R a b =
       C { a :: a -- ^ 'a' 字段说明
         , b :: b -- ^ 'b' 字段说明
         }

     f :: Int    -- ^ 'Int' 参数说明
       -> Float  -- ^ 'Float' 参数说明
       -> IO ()  -- ^ 返回值说明

- ``-- |``\ 也可对部分种类的声明的一部分进行文档注释，但需要置于对象的上一行；

  .. code-block::

     data T a b
       -- | 'C1' 构造器说明
       = C1 a b
       -- | 'C2' 构造器说明
       | C2 a b

     data R a b =
       C { -- | 'a' 字段说明
           a :: a,
           -- | 'b' 字段说明
           b :: b
         }

模块描述
--------

.. code-block::

   {-|
     <label> : <description>
     ...
     <long-description>
   -}

- 可对整个模块本身进行文档注释；
- 单行和多行注释均可；
- 标签：对文档进行说明的字段；

  - 可选，但使用时必须\ **按顺序出现**\ ；

    .. code-block::

       {-|
         Copyright : (c) J. Random, 2077
         License   : GPL-3
       -}

  - 支持多行说明，但连续行的缩进必须大于标签所在行的缩进；

    .. code-block::

       -- | Copyright : (c) J. Random, 2077
       --                   F. Foobar, 2077

- 字段：

  - ``Module``\ ：模块名；
  - ``Description``\ ：对模块功能的简短描述；
  - ``Copyright``\ ：版权信息；
  - ``License``\ （\ ``Licence``\ ）：模块使用许可证；
  - ``Maintainer``\ ：模块维护者邮箱；
  - ``Stability``\ ：模块稳定性（\ ``stable``\ 、\ ``experimental``\ 等）；
  - ``Portability``\ ：操作系统或 GHC 版本，因人而异；

- 说明：在字段之后，可对模块进行整体性说明，所有 Haddock 标记语法在该部分均有效；

.. code-block::

   {-|
     Module      : Sample
     Description : 对模块的简短描述
     Copyright   : (c) J. Random, 2077
                       F. Foobar, 2077
     License     : MIT
     Maintainer  : random@email.com
     Stability   : experimental
     Portability : POSIX

     此处为模块的详细说明，可包含其他@标记语法@。
   -}
   module Sample where

文档结构
--------

排版顺序
~~~~~~~~

- 文档排版的顺序与导出的顺序相同；

.. code-block::

   module Image
     ( -- * 图像导入函数
       -- 此处为描述 ...
       readImage
     , readPngImage
     , readGifImage
     ...
     ) where

   -- 代码实现的顺序可以和导出的顺序不同。
   -- 最终生成的文档顺序以导出时定义的顺序为准。

   readImage :: FilePath -> IO Image
   readImage = ...

   readGifImage :: FilePath -> IO Image
   readGifImage = ...

   readPngImage :: FilePath -> IO Image
   readPngImage = ...

模块重导出
~~~~~~~~~~

- 当重新在导出列表中导出另一个模块时，若模块完全导入，则 Haddock 会生成链接到该模块的链接；

  .. code-block::

     module A (module B, module C) where
     import B
     import C
     -- 生成跳转到模块 'B' 和 'C' 的联链接

- 若模块不完全导入（\ ``hiding``\ 等），则 Haddock 会生成导入部分对象的文档；

  .. code-block::

     module A (module B, module C) where
     import B hiding (f)
     import C (a, b)
     -- 生成模块 'B' 中除 'f' 外所有对象的文档
     -- 模块 'C' 中只生成 'a' 和 'b' 的文档

章节标题
~~~~~~~~

.. code-block::

   -- * <heading>
   -- ** <heading>

- ``-- *``\ ：定义一个章节标题，星号越多标题层级越低；

  .. code-block::

     module Image
       ( -- * 函数
         -- ** 图像导入函数
         readImage
       , readPngImage
       , readGifImage
         -- ** 图像导出函数
       , writeImage
       , writePngImage
       , writeGifImage
       ) where

- 若定义了标题，则 Haddock 会自动在模块文档的顶部生成目录；
- 若定义了导出列表，则标题必须在导出列表中定义；若未定义导出列表，则可直接定义在模块体中；

  .. code-block::

     module Image where
     -- * 函数
     -- ** 图像导入函数
     readImage :: ...
     ...

     -- ** 图像导出函数
     writeImage :: ...
     ...

命名文档块
~~~~~~~~~~

.. code-block::

   -- $<name>
   -- ...

- 文档块：和任何声明都无关联的文档说明，可置于模块描述中，或与标题相关联；
- 关联方式：

  - 用\ ``-- |``\ 直接书写在导出列表中（无命名）；

    .. code-block::

       module Sample
         ( -- * 章节标题
           -- | 和特定实体无关联的文档说明
           ...
         ) where

  - 将文档块置于模块体中，使用\ ``-- $<name>``\ 命名并引用文档块，此时命名文档块必须位于所有导入语句之后；

    .. code-block::

       module Sample
         ( -- * 章节标题
           -- $doc
           ...
         ) where
       import ...
       -- $doc
       -- 一大段文档说明。
       -- 用名称 $doc 关联。

  - 直接置于模块体中，使用\ ``-- $<name>``\ 命名并引用文档块，自动与前一个标题相关联（若不命名，而是直接使用\ ``-- |``\ ，则自动与同一标题下的下一个声明相关联）；

    .. code-block::

       module Sample where
       -- * 章节标题
       -- $doc
       -- 一大段文档说明。
       -- 用名称 $doc 与前一标题关联。

超链接和重导出
~~~~~~~~~~~~~~

- 当 Haddock 生成文档时，导入的对象会链接到各自对应的定义处；
- 每个对象都有一个家模块，Haddock 在链接对象时会指向其家模块，除非当前模块重导出了该对象，此时 Haddock 会链接到当前模块；
- 家模块：

  - 若模块 A 和 B 都导出了对象，且模块 A 导入了模块 B，则模块 B 为家模块；
  - 若模块具有\ ``hide``\ :ref:`属性 <haddock:模块属性>`\ ，则永远都不是家模块；
  - 若模块具有\ ``not-home``\ :ref:`属性 <haddock:模块属性>`\ ，则不存在其他模块可供链接时链接该模块；
  - 若多个模块都符合上述条件，则随机选择一个；
  - 若无模块符合上述条件，则发出警告；

.. code-block::

   module A (T) where    -- 'T' 的家模块
   data T a = C a

   module B (f) where
   import A
   f :: T Int -> Int     -- 超链接至 A.T
   f (C i) = i

   module C (T, f) where -- 本地超链接至 C.T
   import A
   import B

模块属性
~~~~~~~~

.. code-block::

   {-# OPTIONS_HADDOCK <attribute>, ... #-}

- 模块属性定义于编译选项中、模块描述之前或之后，多个属性用逗号分隔；
- 属性：

  - ``hide``\ ：生成文档时忽略该模块，但其他模块重导出对象时同样生成文档；
  - ``prune``\ ：生成文档时忽略没有文档注释的对象；
  - ``ignore-exports``\ ：忽略导出列表；
  - ``not-home``\ ：对导出的对象来说，当前模块不是家模块，除非没有其他模块能作为对象的家模块；
  - ``show-extensions``\ ：仅当导出格式支持时，在文档中显示当前模块使用的\ :tr:`编译选项 (extensions)`；

标记
----

段落
~~~~

- 一或多个空行分隔段落；

字符实体
~~~~~~~~

.. code-block::

   -- &#<decimals>;
   -- &#x<hexadecimals>;

- 虽然 Haddock 源文件支持 Unicode 字符集，但只有 ASCII 字符集具有可移植性；
- Haddock 支持 SGML 格式的字符实体引用，可用十进制或十六进制定义字符实体；

.. code-block::

   -- &#x3bb; is the lower case lambda letter.

代码块
~~~~~~

.. code-block::

   -- @
   -- <code>
   -- @
   -- > <code>

- 使用\ ``@``\ 包围段落可将段落或字符串指定为代码块，\ ``@``\ 中其他标记语法均有效；
- 在段落所有行前使用\ ``>``\ 可将段落指定为代码块，\ ``>``\ 中不能使用其他标记语法；

.. code-block::

   -- @f x = x + x@ 是一个代码块
   -- > g x = x * 42

例子
~~~~

.. code-block::

   -- >>> <code>

- ``>>>``\ 定义一个 REPL 例子；

.. code-block::

   -- | 以下给出两个例子：
   --
   -- >>> fib 10
   -- 55
   --
   -- >>> putStrLn "foo\nbar"
   -- foo
   -- bar

属性
~~~~

- ``prop>``\ 定义一个属性，可供第三方软件提取并使用；

.. code-block::

   -- | 加法具有交换性：
   --
   -- prop> a + b = b + a

标题
~~~~

.. code-block::

   -- = <heading>
   -- == <heading>
   -- = __<collapse>__

- ``=``\ 定义标题，\ ``=``\ 越多标题级别越低，最多6个\ ``=``\ ；
- 标题之后可直接后接段落级别的内容，不用另起新行；
- ``= __<collapse>__``\ 定义可折叠区块，改变\ ``=``\ 数量可改变级别；

  - 可折叠区域一直延伸至下一个同级别及以上标题或注释结束；

.. code-block::

   -- |
   -- === __例子：__
   -- >>> foo 1 2
   -- 3
   --
   -- ==== 落入折叠区域中
   -- 一些例子
   --
   -- === 不属于折叠区域
   -- 更多内容

链接
~~~~

.. code-block::

   -- '<module>.<identifier>'
   -- "<module>"
   -- <<url>>

- 标识符：

  - 用单引号\ ``''``\ 、反引号\ ``````\ 或单引号和反引号（\ ```'``\ 或\ ``'```\ ）包围标识符可创建指向该标识符定义的本地链接；
  - 创建链接时 Haddock 会检查标识符是否存在于本地，不存在则不创建链接；
  - 添加模块名可链接至范围外的标识符，生成文档后不会显示模块名；
  - 引号中存在特殊字符时不用特意转义，可直接使用；
  - 不能直接引用中缀和前缀运算符，可先用\ ``@``\ 包围转换为代码块，再在\ ``@``\ 内引用运算符；

  .. code-block::

     -- | 标识符 'M.T' 不在范围内。
     -- 不需要对 foo' 进行转义：'foo''。
     -- 前缀运算符 @('++')@ 和中缀标识符 @'`elem`'@。

- 模块：用双引号\ ``""``\ 包围模块名可创建指向该模块的超链接；

  - 模块不会检查模块的存在情况，始终创建链接；

  .. code-block::

     -- | 超链接至模块 "Foo"。

- URL：用尖括号\ ``<>``\ 包围 URL 可创建链接；

  - Haddock 会检测\ ``http://``\ 、\ ``ssh://``\ 等协议头，并自动将其转换为超链接；

锚点
~~~~

.. code-block::

   -- #<label>#
   -- "<module>#<label>"

- ``##``\ 包围定义一个被引用的锚点，生成文档后不会显示；
- ``"<module>#<label>"``\ 创建一个指向锚点的链接，模块名不一定为当前模块；

.. code-block::

   -- |#anc#这是一个锚点。
   -- |点击 "Sample#anc" 跳转锚点。

强调
~~~~

.. code-block::

   -- /<text>/

- ``//``\ 包围要强调的文本，默认样式为斜体；
- ``//``\ 中其他标记语法均有效，\ ``/``\ 需转义；

.. code-block::

   -- |强调的内容: /note/

粗体
~~~~

.. code-block::

   -- __<text>__

- ``____``\ ：包围粗体文本；
- ``____``\ 中不需要对\ ``_``\ 进行转义；

.. code-block::

   -- |粗体文本: __this_is_bold__

列表
~~~~

.. code-block::

   -- * <item>
   -- (1) <item>
   -- 1. <item>
   -- [<identifier>]: <description>

- 无序列表：段落前的\ ``*``\ 定义无序列表；
- 有序列表：段落前的\ ``(n)``\ 或\ ``n.``\ 定义有序列表；
- 术语列表：\ ``[]``\ 定义术语项（可使用其他标记语法），分号后定义描述；
- 列表项可跨行；
- 列表可嵌套，嵌套列表缩进为 4 个空格；
- 位于同一缩进层次的列表项属于同一列表级别；

.. code-block::

   -- | 列表：
   --
   -- * 列表首项。
   -- 该行属于上一列表项。
   --
   --     (1) 嵌套列表。
   --     缩进列表。
   --
   --         [@foo@]: @foo@ 的说明。
   --         该实体为函数。
   --
   --         > foo x y = x + y
   --
   --     2. 同一缩进层级的列表项。
   --
   -- * 顶级列表项。
   -- 缩进列表项。

图像
~~~~

.. code-block::

   -- ![<description>](<path>)

- Haddock 支持 Markdown 内联图像语法；

LaTeX
~~~~~

.. code-block::

   -- \(<math>\)
   -- \[
   -- <math>
   -- \]

- Haddock 支持 LaTeX 公式语法，\ ``\(\)``\ 定义内联公式，\ ``\[\]``\ 定义多行公式；

表格
~~~~

.. code-block::

   -- +----+----+
   -- |    |    |
   -- +====+====+
   -- |    |    |
   -- +----+----+

- Haddock 支持 reST 表格语法；

元数据
~~~~~~

.. code-block::

   -- @since <version>

- ``@since``\ 注释后只能接模块的版本号，表示对象最早在模块中定义时的版本号；
- ``@since``\ 为段落级别；
- 多个\ ``@since``\ 只有最后一个有效；

.. code-block::

   -- | 备注：'elem' 通常用作中缀标识符。
   --
   -- @since 4.8.0.0

.. _官网: https://www.haskell.org/haddock/
