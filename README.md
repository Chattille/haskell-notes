# Haskell 学习笔记

Haskell 个人学习笔记，基于 reStructuredText 与 Sphinx。

笔记结构基本依照《[Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)》，习题取自 [Exercism](https://exercism.org/)。

[在线预览](https://chattille.github.io/haskell-notes/)。

## 生成 HTML 文档

### 依赖

- [Sphinx](https://www.sphinx-doc.org/zh_CN/master/)：推荐安装版本`sphinx>=6.2.0,<7.0.0`；
- [Furo](https://pradyunsg.me/furo/)：Sphinx 主题；
- [sphinx-copybutton](https://github.com/executablebooks/sphinx-copybutton)（可选）：代码块复制按钮：
- [结巴](https://github.com/fxsjy/jieba)（可选）：中文搜索；

```bash
$ pip install 'sphinx>=6.2.0,<7.0.0' furo sphinx-copybutton jieba
```

### 生成

```bash
$ make html
```
