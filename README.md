# Haskell 学习笔记

Haskell 个人学习笔记，基于 reStructuredText 和 Sphinx。

笔记结构基本依照《[Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)》，习题取自 [Exercism](https://exercism.org/)。

## 生成 HTML 文档

### 依赖

- [Sphinx](https://www.sphinx-doc.org/zh_CN/master/)：

  ```
  $ pip install sphinx
  ```

- Sphinx 主题 [Furo](https://pradyunsg.me/furo/)：

  ```
  $ pip install furo
  ```

- 代码块复制按钮（可选）：

  ```
  $ pip install sphinx-copybutton
  ```

- 中文搜索（可选）：

  ```
  $ pip install jieba
  ```

### 生成

```
$ make html
```
