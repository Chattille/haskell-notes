.. highlight:: console

=====
Stack
=====

.. note::

   本章节选、整理并翻译自 `Stack 文档`_\ 。

简介
====

- Stack：跨平台的 Haskell 项目管理工具，包括 GHC 安装、项目编译、依赖安装等；
- Stack 在 Cabal 的基础上做了改进，是 Cabal 的增强版；
- Cabal 过于复杂，更推荐使用 Stack 进行包和项目管理；

命令
====

.. code-block::

   $ stack [options] COMMAND|FILE

- 运行 Stack 命令行；

.. rubric:: 参数

.. program:: stack

.. option:: COMMAND

   子命令。

.. option:: FILE

   指定文件名。

.. rubric:: 选项

.. program:: stack

.. option:: --resolver <resolver>

   覆盖项目指定的\ :tr:`解析器 (resolver)`\ ，使用该选项指定的\ :ref:`解析器 <stack:stackage>`\ ，解析器格式与配置文件一致。

``setup``
---------

.. code-block::

   $ stack setup [GHC_VERSION] [options]

- 下载指定 GHC；

.. rubric:: 参数

.. program:: stack setup

.. option:: GHC_VERSION

   GHC 版本，默认与当前解析器一致。

``init``
--------

.. code-block::

   $ stack init [DIR] [options]

- 创建 Stack 项目配置文件\ :file:`stack.yaml`\ ；
- 快照：

  - ``init``\ 命令会寻找当前目录及子目录中的\ :file:`.cabal`\ 文件，并计算依赖和版本；
  - ``init``\ 命令会找到最佳\ :ref:`快照 <stack:stackage>`\ ，使编译需要的外部依赖最少；
  - ``init``\ 命令会以最新 :ref:`LTS <stack:stackage>`\ 、最新 :ref:`Nightly <stack:stackage>`\ 、其他 LTS 的顺序寻找最佳快照；

.. rubric:: 参数

.. program:: stack init

.. option:: DIR

   生成配置文件的目录名，默认当前目录；

.. rubric:: 选项

.. program:: stack init

.. option:: --force

   强制覆盖已有的\ :file:`stack.yaml`\ 。

.. option:: --ignore-subdirs

   忽略指定子目录下的\ :file:`.cabal`\ 文件。

.. option:: --omit-packages

   排除冲突的包。

.. code-block::

   $ stack init
   Looking for .cabal or package.yaml files to use to init the project.
   We didn't find any local package directories
   You may want to create a package with "stack new" instead
   Create an empty project for now
   ...

``new``
-------

.. code-block::

   $ stack new PACKAGE_NAME [option] [TEMPLATE_NAME] [options]

- 创建 Haskell 项目；

.. rubric:: 参数

.. program:: stack new

.. option:: PACKAGE_NAME

   Haskell 包名，由字母数字和短横杠\ ``-``\ 组成。

.. option:: TEMPLATE_NAME

   包结构的模板，可以是 URL、本地文件名或\ ``[[service]:username/]template``\ （\ ``service``\ 可以为\ ``github``\ 、\ ``gitlab``\ 或\ ``bitbucket``\ ）形式，默认为\ ``new-template``\ 。

.. code-block::

   $ stack new helloworld new-template
   Downloading template "new-template" to create project "helloworld"
   in helloworld/ ...

   Looking for .cabal or package.yaml files to use to init the project.
   Using cabal packages:
   - helloworld/

   ...

``build``
---------

.. code-block::

   $ stack build [TARGET] [options]

- 编译 Haskell 项目；
- 编译：

  - 编译后会生成\ :file:`stack.yaml.lock`\ 文件和\ :file:`.stack-work/`\ 目录；
  - 编译后产生的中间文件和可执行文件都在\ :file:`.stack-work/`\ 目录下；
  - ``build``\ 命令产生的可执行文件默认名为\ :file:`{PACKAGE_NAME}-exe`\ ；

.. rubric:: 参数

.. program:: stack build

.. option:: TARGET

   编译对象，默认为所有本地的包。可以为以下格式：

   - ``PACKAGE``\ ：可指定包名；

     .. code-block::

        $ stack build helloworld

   - ``PACKAGE-VERSION``：可指定包标识符，即包名加版本号；

     .. code-block::

        $ stack build helloworld-1.4.14

   - ``[PACKAGE[:TYPE]]:COMPONENT``\ ：可指定单个构成部分；

     .. code-block::

        $ stack build helloworld:test:helloworld-test
        $ stack build helloworld:helloworld-test
        $ stack build :helloworld-test

     - 构成部分：可通过\ ``stack ide targets``\ 命令查看；

       - ``:lib``\ ：库；
       - ``[:exe]:PACKAGE-exe``\ ：可执行文件；
       - ``[:test]:PACKAGE-test``\ ：测试组；

     - 指定其中一个部分不会编译其他部分；

   - ``DIR``\ ：可指定单个目录，编译该目录下的所有包和子包；

.. rubric:: 选项

.. program:: stack build

.. option:: --dry-run

   显示编译后产生的影响，但不编译。

.. option:: --ghc-options

   指定 GHC 编译选项。

.. option:: --[no-]haddock

   禁用/启用当前目录的文档生成，默认禁用。

.. option:: --[no-]test

   禁用/启用当前目录的测试，默认禁用。

.. option:: --[no-]copy-bins

   禁用/启用将当前目录的可执行文件复制到指定二进制目录，默认禁用。

.. option:: --[no-]bench

   禁用/启用当前目录的标杆分析，默认禁用。

.. option:: --[no-]test

   禁用/启用当前目录的测试，默认禁用。

.. option:: --no-run-tests

   编译测试组后禁止运行测试组。

.. option:: --no-run-benchmarks

   编译标杆分析组后禁止运行标杆分析。

.. option:: --skip <ARG>

   编译时跳过指定构成部分。

.. option:: --coverage

   生成代码覆盖报告。

.. code-block::

   $ stack build
   Building all executables for `helloworld' once. After a successful
   build of all of them, only specified executables will be rebuilt.
   helloworld> configure (lib + exe)
   Configuring helloworld-0.1.0.0...
   helloworld> build (lib + exe)
   Preprocessing library for helloworld-0.1.0.0..
   Building library for helloworld-0.1.0.0..
   [1 of 2] Compiling Lib
   [2 of 2] Compiling Paths_helloworld
   ...

``ghc``
-------

.. code-block::

   $ stack ghc [options]

- 运行 GHC 编译器；

.. rubric:: 选项

.. program:: stack ghc

.. option:: -- <argument(s)>

   指定 GHC 编译器的参数。

.. option:: --cwd <dir>

   运行前指定当前工作目录。

.. option:: --package <package(s)>

   指定额外的包。

.. code-block::

   $ stack ghc -- app/Main.hs
   [1 of 1] Compiling Main             ( app/Main.hs, app/Main.o )
   Linking app/Main ...

``runghc`` 和 ``runhaskell``
----------------------------

.. code-block::

   $ stack runghc [options]

- 运行\ ``runghc``\ 命令；
- ``stack runhaskell``\ 是\ ``stack runghc``\ 命令的别名；
- ``runghc``\ 命令可用于\ :ref:`脚本解释 <stack:\`\`script\`\`>`\ ；

.. rubric:: 选项

.. program:: stack runghc

.. option:: -- <argument(s)>

   指定\ ``runghc``\ 命令的参数。

.. option:: --cwd <dir>

   运行前指定当前工作目录。

.. option:: --package <package(s)>

   指定额外的包。

``ghci``
--------

.. code-block::

   $ stack ghci [TARGET/FILE] [options]

- 运行 GHCi；

.. rubric:: 参数

.. program:: stack ghci

.. option:: TARGET/FILE

   指定加载的包或文件，默认为所有本地包。


.. rubric:: 选项

.. program:: stack ghci

.. option:: --ghci-options <options>

   指定 GHCi 选项。

.. option:: --ghc-options <options>

   指定 GHC 选项。

.. option:: --with-ghc <ghc>

   指定使用的 GHC 编译器版本。

.. option:: --[no-test]

   禁用/启用测试组，默认禁用。

.. option:: --[no-bench]

   禁用/启用标杆分析，默认禁用。

``exec``
--------

.. code-block::

   $ stack exec COMMAND [-- ARGUMENT(S)] [options]

- 添加、修改环境变量并执行命令；
- 命令执行：

  - ``build``\ 命令后产生的可执行文件可通过\ ``exec``\ 命令执行，\ ``exec``\ 命令会自动解析可执行文件的地址，不用手动指定；
  - ``exec ghci``\ 命令可用于\ :ref:`脚本解释 <stack:\`\`script\`\`>`\ ；


.. rubric:: 参数

.. program:: stack exec

.. option:: COMMAND

   要执行的命令。

.. code-block::

   $ stack exec helloworld-exe
   someFunc

``script``
----------

.. code-block::

   $ stack script [option] FILE [options]

- 解释 Haskell 脚本；
- 解释器：

  - Stack 可用作 Haskell 源文件的解释器；

    .. code-block::

       $ stack Main.hs
       Hello World

  - ``script``\ 命令会忽略所有配置文件；
  - 使用\ ``script``\ 命令时，必须指定全局选项\ :option:`--resolver <stack --resolver>`\ ；
  - Shebang：

    - 可以为 Haskell 源文件指定 Shebang，Shebang 后跟特殊注释以指定编译行为；

      .. code-block:: haskell

         #!/usr/bin/env stack
         -- stack --resolver lts-18.23 script --package random

    - 特殊注释可以为多行注释；

      .. code-block:: haskell

         #!/usr/bin/env stack
         {- stack
            --resolver lts-18.23
            script
            --package random
          -}

    - 若不指定 Shebang 和特殊注释，则 Stack 默认使用\ ``runghc``\ 命令执行文件；

  - 其他命令：某些命令同样可用于特殊注释中；

    - ``runghc``\ 或\ ``runhaskell``\ 命令：不推荐使用，更推荐\ ``script``\ 命令；

      .. code-block:: haskell

         #!/usr/bin/env stack
         {- stack
            --resolver lts-18.23
            --install-ghc
            runghc
            --package random
            --
            -hide-all-packages
          -}

    - ``exec ghci``\ 命令：将文件加载到 GHCi 中；

      .. code-block:: haskell

         #!/usr/bin/env stack
         {- stack
            --resolver lts-18.23
            --install-ghc
            exec ghci
            --package random
          -}

.. rubric:: 参数

.. program:: stack script

.. option:: FILE

   Haskell 源文件。

.. rubric:: 选项

.. program:: stack script

.. option:: --ghc-options <options>

   GHC 选项。

.. option:: --package <package(s)>

   安装指定的包。

.. option:: --extra-dep <package-version>

   指定不在快照中的依赖。

.. option:: --compile

   编译文件时不作优化，并运行。

.. option:: --optimize

   编译文件时优化，并运行。

.. option:: --no-run

   编译后不运行。

``test``
--------

.. code-block::

   $ stack test [TARGET] [options]

- 编译项目并运行测试组，实际是\ :option:`stack build --test <stack-build --[no-]test>`\ 命令的别名；

.. rubric:: 参数

- 与\ ``build``\ 命令相同

.. code-block::

   $ stack test
   helloworld-0.1.0.0: unregistering (components added: test:helloworld-test)
   helloworld> configure (lib + exe + test)
   Configuring helloworld-0.1.0.0...
   helloworld> build (lib + exe + test)
   Preprocessing library for helloworld-0.1.0.0..
   Building library for helloworld-0.1.0.0..
   ...
   helloworld> test (suite: helloworld-test)

   Test suite not yet implemented

   helloworld> Test suite helloworld-test passed
   Completed 2 action(s).

``install``
-----------

.. code-block::

   $ stack install [TARGET] [options]

- 编译项目并将可执行文件复制到指定二进制目录下，实际是\ :option:`stack build --copy-bins <stack-build --[no-]copy-bins>`\ 命令的别名；

.. rubric:: 参数

- 与\ ``build``\ 命令相同

.. code-block::

   $ stack install
   Building all executables for `helloworld' once. After a successful build
   of all of them, only specified executables will be rebuilt.
   ...

   Copied executables to /Users/chattille/.local/bin:
   - helloworld-exe

``haddock``
-----------

.. code-block::

   $ stack haddock [TARGET] [options]

- 编译项目并生成文档，实际是\ :option:`stack build --haddock <stack-build --[no-]haddock>`\ 命令的别名；

.. rubric:: 参数

- 与\ ``build``\ 命令相同

``bench``
---------

.. code-block::

   $ stack bench [TARGET] [options]

- 编译项目并进行标杆分析，实际是\ :option:`stack build --bench <stack-build --[no-]bench>`\ 命令的别名；
- 参数：与\ ``build``\ 命令相同；

``clean``
---------

.. code-block::

   $ stack clean [PACKAGE] [options]

- 清除指定包下产生的各种编译文件，即\ :file:`.stack-work/dist/`\ 目录；

.. rubric:: 参数

.. program:: stack clean

.. option:: PACKAGE

   包名，默认为所有包。

.. rubric:: 选项

.. program:: stack clean

.. option:: --full

   清除整个\ :file:`.stack-work/`\ 目录。

.. code-block::

   $ tree -L 1 .stack-work
   .stack-work
   ├── dist
   ├── install
   ├── stack.sqlite3
   └── stack.sqlite3.pantry-write-lock
   $ stack clean
   $ tree -L 1 .stack-work
   .stack-work
   ├── install
   ├── stack.sqlite3
   └── stack.sqlite3.pantry-write-lock

``purge``
---------

.. code-block::

   $ stack purge [options]

- 清除整个\ :file:`.stack-work/`\ 目录，但不包括\ ``install``\ 命令复制的可执行文件，可将项目还原到未编译状态，实际是\ :option:`stack clean --full`\ 命令的别名；

.. code-block::

   $ ls -A
   .gitignore    LICENSE   app              src             test
   .stack-work   README.md helloworld.cabal stack.yaml
   ChangeLog.md  Setup.hs  package.yaml     stack.yaml.lock
   $ stack purge
   $ ls -A
   .gitignore    README.md helloworld.cabal stack.yaml
   ChangeLog.md  Setup.hs  package.yaml     stack.yaml.lock
   LICENSE       app       src              test

``ls``
------

.. code-block::

   $ stack ls COMMAND [SUBCOMMAND] [options]

- 列出指定要素；

.. rubric:: 参数

.. program:: stack ls

.. option:: COMMAND

   命令。可以为：

   - ``snapshots``\ ：显示快照；

     .. rubric:: 参数

     .. program:: stack ls snapshots

     .. option:: COMMAND

        子命令。可以为：

        - ``remote``\ ：显示远程快照；
        - ``local``\ ：显示本地快照，默认命令；

     .. rubric:: 选项

     .. program:: stack ls snapshots

     .. option:: -l, --lts

        只显示 LTS 快照。

     .. option:: -n, --nightly

        只显示 Nightly 快照。

   - ``dependencies``\ ：列出依赖包；

     .. rubric:: 参数

     .. program:: stack ls dependencies

     .. option:: COMMAND

        子命令。可以为：

        - ``text``\ ：以文本格式输出，默认命令；
        - ``tree``\ ：以树状图格式输出；
        - ``json``\ ：以 JSON 格式输出；

     .. rubric:: 选项

     .. program:: stack ls dependencies

     .. option:: --depth <depth>

        指定依赖深度。

     .. option:: --prune <packages>

        排除指定包，多个包用逗号分隔。

   - ``stack-colors``\ 或\ ``stack-colours``\ ：列出 Stack 输出时使用的颜色；

``list``
--------

.. code-block::

   $ stack list [PACKAGE] [options]

- 列出包在快照中的 ID；

.. rubric:: 参数

.. program:: stack list

.. option:: PACKAGE

   包名，默认为本地所有包。

``unpack``
----------

.. code-block::

   $ stack unpack PACKAGE [options]

- 下载指定包；

.. rubric:: 参数

.. program:: stack unpack

.. option:: PACKAGE

   要下载的包名。

.. rubric:: 选项

.. program:: stack unpack

.. option:: --to <arg>

   下载到该目录的子目录中。

``path``
---------

.. code-block::

   $ stack path [options]

- 打印 Stack 的路径信息；

.. rubric:: 选项

.. program:: stack path

.. option:: --bin-path

   ``PATH``\ 环境变量。

.. option:: --programs

   Stack 安装的 GHC 编译器路径。

.. option:: --stack-root

   Stack 的根目录。

.. option:: --project-root

   当前项目的根目录。

.. option:: --local-hoogle-root

   本地 Hoogle 的根目录。

.. option:: --local-doc-root

   当前项目的文档根目录。

.. option:: --local-bin

   二进制目录，\ ``install``\ 命令会将可执行文件安装到此目录。

.. option:: --snapshot-pkg-db

   :ref:`快照数据库 <stack:数据库>`\ 路径；

.. option:: --local-pkg-db

   :ref:`本地数据库 <stack:数据库>`\ 路径；

.. option:: --global-pkg-db

   :ref:`全局数据库 <stack:数据库>`\ 路径；

``ide``
-------

.. code-block::

   $ stack ide [options] COMMAND

- IDE 相关命令；

.. rubric:: 参数

.. program:: stack ide

.. option:: COMMAND

   子命令。可以为：

   - ``targets``\ ：打印所有可编译 Stack 对象；
   - ``packages``\ ：打印所有可加载的本地包；

``templates``
-------------

.. code-block::

   $ stack templates [options]

- 显示模板帮助信息；

``dot``
-------

.. code-block::

   $ stack dot [options] [TARGET] [options]

- 用 Graphviz_ 格式显示依赖关系；

.. rubric:: 参数

.. program:: stack dot

.. option:: TARGET

   显示该目标的依赖关系，默认为所有本地包。

.. rubric:: 选项

.. program:: stack dot

.. option:: --[no-]external

   禁用/启用外部依赖，默认禁用。

.. option:: --depth <depth>

   依赖关系的深度，默认无限。

.. option:: --prune <packages>

   去除指定包，多个包用逗号分隔。

.. option:: --test

   包括测试组的依赖。

.. option:: --bench

   包括标杆分析组的依赖。

``sdist``
---------

.. code-block::

   $ stack sdist [DIR] [options]

- 生成包的\ :file:`.tar`\ 文件以供上传分享；

.. rubric:: 参数

.. program:: stack sdist

.. option:: DIR

   生成\ :file:`.tarball`\ 文件的目录，默认为当前目录。

.. rubric:: 选项

.. program:: stack sdist

.. option:: --tar-dir <arg>

   将所有\ :file:`.tar`\ 文件复制到该目录。

``upload``
----------

.. code-block::

   $ stack upload [DIR] [options]

- 将包上传到 :ref:`Hackage <stack:stackage>`；

.. rubric:: 参数

.. program:: stack upload

.. option:: DIR

   要上传的目录，默认为当前目录。

``update``
----------

.. code-block::

   $ stack update [options]

- 下载并更新包索引；

``upgrade``
-----------

.. code-block::

   $ stack upgrade [options]

- 升级 Stack 到最新版本；

项目结构
========

- ``stack new``\ 命令默认使用模板\ ``new-template``\ ；
- 目录结构：

  - :file:`.gitignore`\ ：指定 Git 版本管理时要忽略的文件；
  - :file:`ChangeLog.md`\ ：项目历史；
  - :file:`LICENSE`\ ：项目使用的许可证；
  - :file:`README.md`\ ：项目简介；
  - :file:`Setup.hs`\ ：Cabal 编译系统的一部分，虽然技术上来说 Stack 不需要该文件，但在 Haskell 世界里推荐包含该文件；
  - :file:`{PACKAGE_NAME}.cabal`\ ：Cabal 编译使用的文件，由\ ``stack build``\ 命令自动更新，不应做修改；
  - :file:`app/`\ ：生成可执行文件的目录；

    - :file:`Main.hs`\ ：主模块，程序的入口；

  - :file:`package.yaml`\ ：包的配置文件；
  - :file:`src/`\ ：主模块使用的各种辅助模块；
  - :file:`stack.yaml`\ ：Stack 编译的配置文件，规定编译行为；
  - :file:`test/`\ ：测试组代码；

Stackage
========

- Hackage_\ ：Cabal 下载包时会从 Haskell 社区的远程库下载，该远程库为 Hackage；
- Stackage_\ ：

  - Stack 下载包时会从自己的远程库下载，该远程库为 Stackage，是一套稳定的从 Hackage 精选的 Haskell 包的集合；
  - Stack 同样支持 Hackage；

- 快照_\ ：

  - Stackage 将多个包打包成一个集合，称为\ :tr:`快照 (snapshot)`\ ，供特定版本的 GHC 使用；
  - 可通过全局选项\ :option:`--resolver <stack --resolver>`\ 或键\ ``resolver``\ 指定快照或 GHC 版本；
  - 快照分为两种：

    .. image:: _images/ghc-lts.png
       :scale: 40%
       :alt: GHC version and corresponding LTS version.
       :align: center

    - LTS_\ ：稳定的精选 Haskell 包集合，每个 GHC 版本都有对应的 :tr:`LTS (long-term support)`\ ；
    - Nightly_\ ：更新的包集合，对应最新 GHC 版本，稳定性不如 LTS；

数据库
======

- 数据库：Haskell 包的数据库，包含包的各种信息，包括编译库、可执行文件、文档和其他文件；

  .. code-block::

     $ ls .stack-work/install/x86_64-osx/.../8.10.7/
     bin   doc   lib   pkgdb

- 数据库结构：Stack 的数据库是分层的，分为全局数据库、快照数据库和本地数据库，可用\ ``ghc-pkg list``\ 命令查看；

  .. code-block::

     $ stack exec -- ghc-pkg list
     /Users/chattille/.ghcup/ghc/.../package.conf.d
         Cabal-3.2.1.0
         array-0.5.4.0
         base-4.14.3.0
         ...
         transformers-0.5.6.2
         unix-2.7.2.2
         xhtml-3000.2.2.1
     /Users/chattille/.stack/snapshots/.../pkgdb
         random-1.2.0
         splitmix-0.1.0.4
     /Users/chattille/.../Test/.stack-work/install/.../pkgdb
         Test-0.1.0.0

  - 全局数据库：GHC 编译器自带的包的数据库，由所有项目共享；
  - 快照数据库：来自快照的包的数据库，储存于\ :file:`~/.stack/snapshots/`\ 目录下，使用相同快照的项目可共享，不同快照不共享；
  - 本地数据库：当前项目的数据库，储存于\ :file:`./.stack-work/install/`\ 目录下，不与其他项目共享；

- 多层数据库有利于在不同项目中复用相同的包，也能防止不同项目间的包污染；

:file:`package.yaml`
====================

- ``dependencies``\ ：指定当前项目的依赖，可用\ ``ls``\ 命令查看依赖；

  .. code-block:: yaml

     dependencies:
       - base >= 4.7 && < 5
       - text  # 在这里添加

- ``extra-deps``\ ：指定不存在于当前快照的依赖；

  .. code-block:: yaml

     extra-deps:
       - acme-missiles-0.3  # 不在 LTS 内

- ``resolver``\ ：指定解析器，若本地不存在对应版本的 GHC 编译器，则自动下载；

  - ``ghc-X.Y.Z``\ ：指定具体 GHC 编译器版本；
  - ``lts-X.Y``\ ：指定 LTS 快照版本，省略\ ``Y``\ 则指定最新\ ``lts-X``\ 版本；
  - ``nightly-YYYY-MM-DD``\ ：指定 Nightly 快照日期；

  .. code-block:: yaml

     resolver: lts-18.23

.. _Graphviz: https://www.graphviz.org/
.. _Hackage: https://hackage.haskell.org/
.. _LTS: https://www.stackage.org/lts/
.. _Nightly: https://www.stackage.org/nightly/
.. _Stackage: https://www.stackage.org/
.. _`Stack 文档`: https://docs.haskellstack.org/en/stable/README/
.. _快照: https://www.stackage.org/snapshots/
