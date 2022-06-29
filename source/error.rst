.. highlight:: haskell
   :linenothreshold: 10

====
异常
====

简介
====

- Haskell 中的某些类型，如\ ``Maybe``\ 和\ ``Either``\ 能够处理部分运算异常，但不够强大；
- Haskell 函数是无副作用的，因此异常处理在 I/O 操作部分更有意义；
- 纯代码部分同样可以抛出异常；

  .. code-block::

     Prelude> 4 `div` 0
     *** Exception: divide by zero

- 因为纯代码部分的状态是透明的，因此异常只能在 I/O 操作部分捕获；
- 应尽量保持 I/O 操作部分代码最小化，利用 Haskell 的类型系统避免异常；

捕获
====

.. hs:module:: System.IO.Error

.. hs:function:: catchIOError :: IO a -> (IOError -> IO a) -> IO a

   若 I/O 操作抛出异常，则将异常传入异常处理函数，并返回处理结果。

   在\ ``base-4.4.0.0``\ 之前，该函数名为\ :hs:func:`catch`\ 。

   ``IOError``\ 类型表示 I/O 操作异常，带有异常信息，该类型的实现取决于语言的实际实现。

   .. code-block::

      import System.Environment
      import System.IO
      import System.IO.Error

      main = toTry `catchIOError` handler

      -- | 程序的核心。
      -- 计算文件有多少行。
      toTry :: IO ()
      toTry = do (fileName : _) <- getArgs
                 contents       <- readFile fileName
                 let lnum = show . length . lines $ contents
                 putStrLn $ "The file has " ++ lnum ++ " lines!"

      -- | 处理 'IOError'。
      -- 若打开文件时出现异常，则打印信息。
      handler :: IOError -> IO ()
      handler _ = putStrLn "Exception!"

.. hs:function:: ioError :: IOError -> IO a
   :module: GHC.IO.Exception

   将捕获的异常重新抛出。

.. hs:function:: isAlreadyExistsError :: IOError -> Bool

.. hs:function:: isAlreadyInUseError :: IOError -> Bool

.. hs:function:: isDoesNotExistError :: IOError -> Bool

.. hs:function:: isEOFError :: IOError -> Bool

.. hs:function:: isFullError :: IOError -> Bool

.. hs:function:: isIllegalOperation :: IOError -> Bool

.. hs:function:: isPermissonError :: IOError -> Bool

.. hs:function:: isResourceVanishedError :: IOError -> Bool

.. hs:function:: isUserError :: IOError -> Bool

   判断异常是否为对应异常。

   .. code-block::

      import System.Environment
      import System.IO
      import System.IO.Error

      main = toTry `catchIOError` handler

      -- | 程序的核心。
      -- 计算文件有多少行。
      toTry :: IO ()
      toTry = do (fileName : _) <- getArgs
                 contents       <- readFile fileName
                 let lnum = show . length . lines $ contents
                 putStrLn $ "The file has " ++ lnum ++ " lines!"

      -- | 处理 'IOError'。
      -- 若有已知异常，则发出警告，否则抛出。
      handler :: IOError -> IO ()
      handler e | isDoesNotExistError e = putStrLn "No such file!"
                | isFullError e         = putStrLn "Disk full!"
                | isIllegalOperation e  = putStrLn "Illegal move!"
                | otherwise             = ioError e

.. hs:function:: ioeGetErrorString :: IOError -> String

.. hs:function:: ioeGetErrorType :: IOError -> IOErrorType

.. hs:function:: ioeGetFileName :: IOError -> Maybe FilePath

.. hs:function:: ioeGetHandle :: IOError -> Maybe GHC.IO.Handle.Types.Handle

.. hs:function:: ioeGetLocation :: IOError -> String

   根据传入的异常获取异常的属性。

.. hs:function:: ioeSetErrorString :: IOError -> String

.. hs:function:: ioeSetErrorType :: IOError -> IOErrorType

.. hs:function:: ioeSetFileName :: IOError -> Maybe FilePath

.. hs:function:: ioeSetHandle :: IOError -> Maybe GHC.IO.Handle.Types.Handle

.. hs:function:: ioeSetLocation :: IOError -> String

   根据传入的异常设置异常的属性。

   .. code-block::

      import System.Environment
      import System.IO
      import System.IO.Error

      main = toTry `catchIOError` handler

      -- | 程序的核心。
      -- 计算文件有多少行。
      toTry :: IO ()
      toTry = do (fileName : _) <- getArgs
                 contents       <- readFile fileName
                 let lnum = show . length . lines $ contents
                 putStrLn $ "The file has " ++ lnum ++ " lines!"

      -- | 处理 'IOError'。
      -- 若有已知异常，则发出警告，否则抛出。
      handler :: IOError -> IO ()
      handler e
        | isDoesNotExistError e = case ioeGetFileName e of
          Just path ->
            let w = show path ++ " " ++ ioeGetErrorString e ++ "."
            in  putStrLn w
          Nothing -> putStrLn "No such file."
        | otherwise = ioError e
