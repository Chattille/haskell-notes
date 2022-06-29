.. highlight:: haskell
   :linenothreshold: 10

====
文件
====

.. hs:module:: System.IO

读取
====

.. hs:function:: getContents :: IO String

   从标准输入接受字符串，遇到文件终止符结束执行。

   :hs:func:`getContents`\ 函数是惰性的，即不会一次性读取所有文件内容。
   
   Haskell 中文本文件默认按行读取，二进制文件默认按字节块读取。

   .. code-block::

      main :: IO ()
      main = do
          cont <- getContents
          putStr $ shortOnly cont

      -- | 过滤掉短行。
      shortOnly :: String -> String
      shortOnly = unlines . filter ((< 10) . length) . lines

.. hs:function:: hSetBuffering :: Handle -> BufferMode -> IO ()

   修改文件读取函数的读取模式。

   ``Handle``\ 类型见\ :hs:func:`openFile`\ 函数。

   ``BufferMode``\ 类型为枚举类型，包含3种模式：

   .. collapse:: 源码

      .. code-block::
         :linenos:

         data BufferMode = NoBuffering
                         | LineBuffering
                         | BlockBuffering (Maybe Int)

   - ``NoBuffering``\ 模式按字符读取，不建议使用；
   - ``LineBuffering``\ 模式按行读取；
   - ``BlockBuffering``\ ：按字节块读取；

     - ``Just Int``\ ：指定块的字节大小；
     - ``Nothing``\ ：由系统决定块的字节大小；

   .. code-block::

      import System.IO

      main :: IO ()
      main = do
          withFile
              "sample.log"
              ReadMode
              (\h -> do
                  hSetBuffering h $ BlockBuffering (Just 2048)
                  cont <- hGetContents h
                  putStr cont
              )

.. hs:function:: interact :: (String -> String) -> IO ()

   对\ :hs:func:`getContents`\ 函数的封装，对读入的文件内容应用函数并输出。

   .. code-block::

      -- | 过滤掉短行并输出。
      main :: IO ()
      main = interact $ unlines . filter ((< 10) . length) . lines

   .. collapse:: 源码

      .. code-block::
         :linenos:

         interact ::  (String -> String) -> IO ()
         interact f = do s <- getContents
                         putStr (f s)

.. hs:function:: openFile :: FilePath -> IOMode -> IO Handle

   接受一个文件路径和\ ``IOMode``\ 类型数据，打开文件并返回文件操作对象。同一文件同时只能有一个文件操作符。

   ``FilePath``\ 类型是\ ``String``\ 的同义词。

   .. collapse:: 源码

      .. code-block::
         :linenos:

         type FilePath = String

   ``IOMode``\ 类型为枚举数据类型，包含4种读写模式。

   .. collapse:: 源码

      .. code-block::
         :linenos:

         data IOMode = ReadMode
                     | WriteMode
                     | AppendMode
                     | ReadWriteMode

   ``Handle``\ 类型代表文件所在地址。

.. hs:function:: hGetContents :: Handle -> IO String

   与\ :hs:func:`getContents`\ 函数类似，但接受一个文件处理对象，读取文件内容并返回为\ ``IO String``\ 。

.. hs:function:: hClose :: Handle -> IO ()

   关闭文件操作对象，任何用\ :hs:func:`openFile`\ 函数打开的文件都必须关闭。

   .. code-block::

      import System.IO

      main :: IO ()
      main = do
          handle <- openFile "sample.log" ReadMode
          contents <- hGetContents handle
          putStr contents
          hClose handle

.. hs:function:: withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r

   将文件路径打开为一个文件操作对象，对该对象应用函数，返回结果并关闭文件操作对象。

   .. code-block::

      import System.IO

      main :: IO ()
      main = do
          withFile
              "sample.log"
              ReadMode
              (\handle -> do
                  contents <- hGetContents handle
                  putStr contents
              )

.. hs:function:: hGetLine :: Handle -> IO String

.. hs:function:: hGetChar :: Handle -> IO Char

   与对应函数相似，但作用于文件操作对象，而非标准输入输出。

   .. code-block::

      import System.IO 

      main :: IO ()
      main = do
          withFile
              "sample.log"
              ReadMode
              (\h -> do
                  line <- hGetLine h
                  putStr line
              )

.. hs:function:: readFile :: FilePath -> IO String

   读取文件，返回内容并关闭操作对象。

   .. code-block::

      main :: IO ()
      main = do
          cont <- readFile "sample.log"
          putStr cont

写入
====

.. hs:function:: hPutStr :: Handle -> String -> IO ()

.. hs:function:: hPutStrLn :: Handle -> String -> IO ()

.. hs:function:: hPrint :: Show a => Handle -> a -> IO ()

   与对应函数相似，但作用于文件操作对象，而非标准输入输出。

.. hs:function:: writeFile :: FilePath -> String -> IO ()

   将字符串写入文件，若文件存在，则覆盖原内容。

   .. code-block::

      import Data.Char

      main :: IO ()
      main = do
          cont <- readFile "sample.log"
          writeFile "alice.log" (map toUpper cont)

.. hs:function:: appendFile :: FilePath -> String -> IO ()

   与\ :hs:func:`writeFile`\ 函数类似，但若文件存在，则追加到文件结尾。

   .. code-block::

      main :: IO ()
      main = do
          todoItem <- getLine
          appendFile "todo.txt" (todoItem ++ "\n")

.. hs:function:: hFlush :: Handle -> IO ()

   强制将缓存区中的内容中文件中读取出来或写入文件。

   Haskell 默认按照\ :hs:func:`hSetBuffering`\ 函数的设置，达到一定数据量后执行读取或写入。

   在关闭文件操作对象前也会进行一次读取或写入。

文件操作
========

.. hs:function:: openTempFile :: FilePath -> String -> IO (FilePath, Handle)

   创建临时文件。创建的临时文件不会自动删除，需要手动删除。

   路径表示临时文件所在目录，若不存在则报错。

   字符串表示临时文件的模板名，函数会自动在名称后添加随机字符，保证其他文件不会被意外覆盖。

   以序对返回临时文件的路径和其操作对象。

   .. code-block::

      import System.IO

      -- | 生成临时文件。
      --
      -- ==== __例子：__
      -- >>> main
      -- ./temp6473-0
      -- {handle: ./temp6473-0}
      --
      -- >>> main
      -- ./temp6473-1
      -- {handle: ./temp6473-1}
      --
      -- >>> :!ls
      -- ./temp6473-0    ./temp6473-1
      main :: IO ()
      main = do
          (tempName, tempHandle) <- openTempFile "." "temp"
          putStrLn tempName
          print tempHandle

.. hs:module:: System.Directory

.. hs:function:: removeFile :: FilePath -> IO ()

   接受文件路径（而不是操作对象）作为参数，删除文件。

.. hs:function:: renameFile :: FilePath -> FilePath -> IO ()

   接受两个文件路径（而不是操作对象）作为参数，重命名文件。

   .. tabs::

      .. tab:: deleteTodo.hs

         .. code-block::

            import Data.List
            import System.Directory
            import System.IO

            main :: IO ()
            main = do
                contents               <- readFile "todo.txt"
                (tempName, tempHandle) <- openTempFile "." "temp"
                -- 打印所有选项
                let tl      = lines contents
                    options = zipWith (\n i -> show n ++ " " ++ i) [1..] tl
                putStrLn "Current todo items you have:"
                mapM_ putStrLn options
                -- 选择一项并删除
                putStrLn "Which one do you want to delete?"
                selStr <- getLine
                -- 将新列表写入临时文件
                let sel     = read selStr
                    newList = delete (tl !! (sel - 1)) tl
                hPutStr tempHandle $ unlines newList -- 备注*
                hClose tempHandle
                putStrLn "Deletion done."
                -- 删除旧文件并重命名临时文件
                removeFile "todo.txt"
                renameFile tempName "todo.txt"

            -- 备注: 不应该使用 'writeFile' 函数，因为该函数会为
            --       临时文件创建新的文件操作对象，可能会因为操作
            --       对象冲突而报错（"resource busy (file is locked)"）

      .. tab:: 控制台

         .. code-block:: console

            $ cat todo.txt
            Walk the dog
            Iron the dishes
            Take salad out of the oven
            $ runghc deleteTodo.hs
            Current todo items you have:
            1 Walk the dog
            2 Iron the dishes
            3 Take salad out of the oven
            Which one do you want to delete?
            2
            Deletion done.
            $ cat todo.txt
            Walk the dog
            Take salad out of the oven

.. hs:function:: copyFile :: FilePath -> FilePath -> IO ()

   将第一个文件的内容复制到第二个文件中。

   .. code-block::

      import System.Directory

      main :: IO ()
      main = copyFile "todo.txt" "todo-old.txt"

.. hs:function:: doesFileExist :: FilePath -> IO Bool

   检查文件是否存在。

   .. code-block::

      import System.Directory
      import System.Environment

      main :: IO ()
      main = do
          (fileName : _) <- getArgs
          fileExists     <- doesFileExist fileName
          if fileExists
              then putStrLn $ fileName ++ " exists."
              else putStrLn $ fileName ++ " does not exist."

.. hs:module:: System.Environment

命令行
======

.. hs:function:: getArgs :: IO [String]

   获取命令的参数，支持模式匹配。

.. hs:function:: getProgName :: IO String

   获取命令名。

   .. tabs::

      .. tab:: PrintArg.hs

         .. code-block::

            import System.Environment
            import System.IO

            main :: IO ()
            main = do
                args <- getArgs
                progName <- getProgName
                putStrLn "=> Arguments:"
                mapM_ putStrLn args
                putStrLn "=> Program name:"
                putStrLn progName

      .. tab:: 控制台

         .. code-block:: console
            :linenos:

            $ runghc PrintArgs.hs first second "multi word args"
            => Arguments:
            first
            second
            multi word args
            => Program name:
            PrintArgs.hs

.. code-block::

   import Data.List
   import System.Directory
   import System.Environment
   import System.IO

   main :: IO ()
   main = do
       (command : args) <- getArgs
       let (Just action) = lookup command dispatch
       action args

   -- | 将子命令分派到对应函数。
   dispatch :: [(String, [String] -> IO ())]
   dispatch =
       [ ("add", add)
       , ("remove", remove)
       , ("view", view)
       , ("bump", bump)
       ]

   -- | 追加一项 todo 到文件结尾。
   add :: [String] -> IO ()
   add [file, line] = appendFile file $ line ++ "\n"

   -- | 从文件中删除一项 todo。
   remove :: [String] -> IO ()
   remove [file, selStr] = do
       contents               <- readFile file
       (tempName, tempHandle) <- openTempFile "." "temp"
       let todoList = lines contents
           sel      = read selStr
           newList  = delete (todoList !! (sel - 1)) todoList
       hPutStr tempHandle $ unlines newList
       hClose tempHandle
       removeFile file
       renameFile tempName file

   -- | 输出所有 todo。
   view :: [String] -> IO ()
   view [file] = do
       contents <- readFile file
       let tl       = lines contents
           numbered = zipWith (\n i -> show n ++ " - " ++ i) [1 ..] $ tl
       mapM_ putStrLn numbered

   -- | 添加一项 todo 到文件开头。
   bump :: [String] -> IO ()
   bump [file, task] = do
       contents               <- readFile file
       (tempName, tempHandle) <- openTempFile "." "temp"
       let newList = task ++ "\n" ++ contents
       hPutStr tempHandle newList
       hClose tempHandle
       removeFile file
       renameFile tempName file

字节串
======

简介
----

- :tr:`字节串 (bytestring)`\ ：一系列连续的字节（八比特），与列表类似；
- 函数求值策略：在编程语言中，函数参数的求值有两种策略：

  - :tr:`传值调用 (call by value)`\ ：参数传入函数前已经完成求值；

    - 优点：CPU 开支少，运算快；
    - 缺点：内存占用多（需要一次性将所有参数都加载到内存中）；

  - :tr:`传名调用 (call by name)`\ ：当函数真正调用时才进行求值（即惰性求值），该过程中编译器会把惰性求值的过程打包为一个名为\ :hs:func:`thunk`\ 的辅助函数；

    - 优点：内存占用少；
    - 确定：CPU 开支多，运算慢（需要频繁调用惰性求值函数）；

- :hs:mod:`Data.ByteString`\ 模块：提供严格字节串相关操作，不进行惰性求值，一次性求值所有字节，因此没有无限长度的严格字节串；
- :hs:mod:`Data.ByteString.Lazy`\ 模块：提供惰性求值，按块求值；

  - 该模块中的字节串按块储存，每块元素占用64字节；
  - 在求值时，惰性字节串会先求值第一个64字节块，剩余所有字节块直到需要之前都不会被求值，因此惰性字节串类似于由严格字节串组成的列表；

函数
----

- :hs:func:`~GHC.List.head`\ 、\ :hs:func:`~GHC.List.tail`\ 、\ :hs:func:`~GHC.List.init`\ 、\ :hs:func:`~Data.Foldable.null`\ 、\ :hs:func:`~Data.Foldable.length`\ 、\ :hs:func:`~GHC.Base.map`\ 、\ :hs:func:`~GHC.List.reverse`\ 、\ :hs:func:`fold*`\ 、\ :hs:func:`~Data.Foldable.concat`\ 、\ :hs:func:`~GHC.List.takeWhile`\ 和\ :hs:func:`~GHC.List.filter`\ 等函数与同名函数功能类似；

- :hs:func:`~System.IO.readFile`\ 、\ :hs:func:`~System.IO.interact`\ 、\ :hs:func:`~System.IO.getContents`\ 等 I/O 操作函数与\ :hs:mod:`System.IO`\ 中的同名函数功能类似；

.. hs:module:: Data.ByteString.Lazy

.. hs:function:: pack :: [Word8] -> ByteString

   接受\ ``Word8``\ 类型列表，并返回字节串。

   ``Word8``\ 类型表示 8 比特无符号整数，即 0-255 闭区间。若数字超过该范围，则 GHC 会抛出警告，并对数字取模。

   .. code-block::

      import qualified Data.ByteString as S
      import qualified Data.ByteString.Lazy as L
      import Data.Word

      exp1 = 23 :: Word8 -- 23
      exp2 = 300 :: Word8
             -- warning: [-overflowed-literals]
             --     Literal 300 is out of the Word8 range 0..255
             -- 44
      exp3 = L.pack [99, 97, 110] -- "can"
      exp4 = L.pack [97 .. 122]   -- "abcdefghijklmnopqrstuvwxyz"

.. hs:function:: unpack :: ByteString -> [Word8]

   将字节串转换为\ ``Word8``\ 类型列表。

   .. code-block::

      exp5 = L.unpack $ L.pack [99, 97, 110] -- [99,97,110]

.. hs:function:: fromChunks :: [Data.ByteString.ByteString] -> ByteString

   将严格字节串列表转换为惰性字节串。

   .. code-block::

      exp6 = L.fromChunks [S.pack [40..42], S.pack [43..45]]
             -- "()*+,-"

.. hs:function:: toChunks :: ByteString -> [Data.ByteString.ByteString]

   将惰性字节串转换为严格字节串列表。

   .. code-block::

      exp7 = L.toChunks $ L.pack [40..45] -- ["()*+,-"]

.. hs:function:: cons :: Word8 -> ByteString -> ByteString

   类似列表的\ ``:``\ 值构造器，将一个字节添加至字节串头，但当字节串头长度不满一个字节块时，将新建一个新的字节块。

   .. code-block::

      exp8 = L.cons 50 . L.pack $ [51 .. 55] -- "234567"
      exp9 = L.toChunks exp8                 -- ["2","34567"]

.. hs:function:: cons' :: Word8 -> ByteString -> ByteString

   :hs:func:`cons`\ 函数的严格版，当字节串头长度不满一个字节块时，添加字节时会尝试将字节和字节串头合并，而非新建字节块。

   .. code-block::

      exp10 = L.cons' 50 . L.pack $ [51 .. 55] -- "234567"
      exp11 = L.toChunks exp10                 -- ["234567"]

.. hs:function:: empty :: ByteString

   返回一个空的惰性字节串。

   .. code-block::

      exp12 = L.empty -- ""

.. hs:module:: Data.ByteString

.. hs:function:: pack :: [Word8] -> ByteString

.. hs:function:: unpack :: ByteString -> [Word8]

.. hs:function:: cons :: Word8 -> ByteString -> ByteString

.. hs:function:: empty :: ByteString

   与\ :hs:mod:`Data.ByteString.Lazy`\ 模块中的同名函数功能类似，但作用于严格字符串。
