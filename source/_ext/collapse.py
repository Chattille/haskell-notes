#!/usr/bin/env python3
"""
Add a collapsible section to an HTML page using a <details> element.
Tailored from 'sphinx-toolbox.collapse'
(https://github.com/sphinx-toolbox/sphinx-toolbox) by domdfcoding.

Usage:

    .. collapse:: [label]
       :fields: [arguments]

       Contents

Example:

    .. collapse: Source
       :class: custom-summary

       .. code-block:: python
          :linenos:

          def hello():
              print("Hello World!")
"""

from typing import Dict, Optional, Sequence

from docutils.nodes import Body, Element, Node, make_id
from docutils.parsers.rst import directives
from docutils.parsers.rst.roles import set_classes
from sphinx.application import Sphinx
from sphinx.util.docutils import SphinxDirective
from sphinx.writers.html import HTMLTranslator

__all__ = ["CollapseNode", "CollapseDirective",
           "visit_collapse_node", "depart_collapse_node", "setup"]


class CollapseNode(Body, Element):
    """A node representing a collapsible section."""

    def __init__(
            self,
            rawsource: str = '',
            label: Optional[str] = None,
            *children,
            **attributes,
    ):
        super().__init__(rawsource, *children, **attributes)
        self.label = label


class CollapseDirective(SphinxDirective):
    """A collapsible section using a <details> element."""

    final_argument_whitespace: bool = True
    has_content: bool = True
    required_arguments: int = 1  # summary
    option_spec: Dict = {
        "class": directives.class_option,
        "name": directives.unchanged,
    }

    def run(self) -> Sequence[Node]:
        """Process the content of the directive."""
        set_classes(self.options)
        self.assert_has_content()
        text = '\n'.join(self.content)
        label = self.arguments[0]  # summary
        # create a node object
        collapse_node = CollapseNode(text, label, **self.options)
        self.add_name(collapse_node)
        collapse_node["classes"].append(f"summary-{make_id(label)}")
        # parse child elements
        self.state.nested_parse(
            self.content, self.content_offset, collapse_node)

        return [collapse_node]


def visit_collapse_node(translator: HTMLTranslator, node: CollapseNode):
    """Visit a CollapseNode and append <details> and <summary>.

    Args:
        translator (HTMLTranslator): Sphinx HTML translator.
        node (CollapseNode): The CollapseNode being visited.
    """
    # opening tag with attribute(s)
    opener = ["details"]
    if node.get("names"):
        names = f"name=\"{' '.join(node['names'])}\""
        opener.append(names)
    if node.get("classes"):
        classes = f"class=\"{' '.join(node['classes'])}\""
        opener.append(classes)

    translator.body.append(
        f"<{' '.join(opener)}>\n<summary>{node.label}</summary>")
    translator.context.append('</details>')


def depart_collapse_node(translator: HTMLTranslator, node: CollapseNode):
    """Depart a CollapseNode and append </details>.

    Args:
        translator (HTMLTranslator): Sphinx HTML translator.
        node (CollapseNode): The CollapseNode being visited.
    """
    translator.body.append(translator.context.pop())


def setup(app: Sphinx) -> Dict:
    """Setup the plugin for collapsible sections.

    Args:
        app (Sphinx): The whole Sphinx application.
    Returns:
        Plugin metadata.
    """
    # latex not supported
    app.add_node(
        CollapseNode,
        html=(visit_collapse_node, depart_collapse_node),
        text=(visit_collapse_node, depart_collapse_node),
        latex=(lambda *args, **kwargs: None, lambda *args, **kwargs: None))
    app.add_directive('collapse', CollapseDirective)

    return {"parallel_read_safe": True}
