#!/usr/bin/env python3
"""
A tabcard containing a set of tabs.

Usage:

    .. tabs::

       ..tab:: tab-title

         content

Example:

    .. tabs::

       .. tab:: tab1

          Content

       .. tab:: tab2

          Content

    would be converted to:

    <div class="tabcard">

        <input checked id="tc-tab-0" name="tabcard-0" type="radio" />
        <label for="tc-tab-0">tab1</label>
        <div class="tc-display">
            Content
        </div>

        <input id="tc-tab-1" name="tabcard-0" type="radio" />
        <label for="tc-tab-1">tab2</label>
        <div class="tc-display">
            Content
        </div>

    </div>
"""

from typing import Dict, Optional, Sequence

from docutils.nodes import Body, Element, Node, container
from docutils.parsers.rst import directives
from docutils.parsers.rst.roles import set_classes
from sphinx.application import Sphinx
from sphinx.util.docutils import SphinxDirective
from sphinx.writers.html import HTMLTranslator

__all__ = ["TabNode", "TabDirective", "TabcardDirective",
           "visit_tab_node", "depart_tab_node", "setup"]


class TabNode(Body, Element):
    """A node representing a tab."""

    def __init__(
            self, rawsource: str = '', title: Optional[str] = None,
            *children, **attributes):
        super().__init__(rawsource, *children, **attributes)
        self.tab_title = title


class TabcardDirective(SphinxDirective):
    """A tabcard containing a set of tabs."""

    has_content: bool = True
    option_spec: Dict = {
        "class": directives.class_option,
        "name": directives.unchanged,
    }

    def run(self) -> Sequence[Node]:
        """Process the content of the directive.

        Returns:
            A list of docutils node that will be inserted into the document.
        """
        set_classes(self.options)
        self.assert_has_content()
        text = "\n".join(self.content)
        # create a node object
        tabcard_node = container(text, **self.options)
        self.add_name(tabcard_node)
        tabcard_node["classes"].append("tabcard")
        # group <input>
        tabcard_node["group"] = f"tabcard-{self.env.new_serialno('tabcard')}"
        # parse child elements
        self.state.nested_parse(
            self.content, self.content_offset, tabcard_node)
        first_tab_node = tabcard_node[0]
        first_tab_node["checked"] = True

        return [tabcard_node]


class TabDirective(SphinxDirective):
    """A tab inside a tabcard."""

    required_arguments: int = 1  # tab title
    has_content: bool = True

    def run(self) -> Sequence[Node]:
        """Process the content of the directive.

        Returns:
            A list of docutils node that will be inserted into the document.
        """
        self.assert_has_content()
        text = "\n".join(self.content)
        title = self.arguments[0]
        # create a node object
        tab_node = TabNode(text, title, **self.options)
        # bind <input> and <label>
        tab_node["input_id"] = self.env.new_serialno("tc-tab")
        # parse child elements
        self.state.nested_parse(self.content, self.content_offset, tab_node)

        return [tab_node]


def visit_tab_node(translator: HTMLTranslator, node: TabNode):
    """Set up the opening tag for a tab.

    Args:
        translator (HTMLTranslator): Sphinx HTML translator.
        node (TabNode): The TabNode being visited.
    """
    checked = " checked" if node.get("checked") else ""
    id = f"tc-tab-{node.get('input_id')}"
    tab_group = node.parent.get("group")

    translator.body.append(
        f"<input id=\"{id}\" name=\"{tab_group}\" type=\"radio\"{checked} />\n"
        f"<label for=\"{id}\">{node.tab_title}</label>\n"
        f"<div class=\"tc-display\">")
    translator.context.append("</div>")


def depart_tab_node(translator: HTMLTranslator, node: TabNode):
    """Add endtag.

    Args:
        translator (HTMLTranslator): Sphinx HTML translator.
        node (TabNode): The TabNode being visited.
    """
    translator.body.append(translator.context.pop())  # tab-display


def setup(app: Sphinx) -> Dict:
    """Setup the plugn.

    Args:
        app (Sphinx): The whole Sphinx application.
    Returns:
        Plugin metadata.
    """
    # latex not supported
    app.add_node(
        TabNode,
        html=(visit_tab_node, depart_tab_node),
        text=(visit_tab_node, depart_tab_node),
        latex=(lambda *args, **kwargs: None, lambda *args, **kwargs: None))
    app.add_directive('tabs', TabcardDirective)
    app.add_directive('tab', TabDirective)

    return {"parallel_read_safe": True}
