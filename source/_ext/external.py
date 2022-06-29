#!/usr/bin/env python3
"""
Open external links in new tabs.

Usage:

    `GitHub <https://github.com/>`_

    would be rendered to:

    <a href="https://github.com/" class="external" target="_blank">GitHub</a>
"""

from docutils.nodes import Node
from sphinx.application import Sphinx
from sphinx.writers.html import HTMLTranslator

__all__ = ["PatchedHTMLTranslator", "setup"]


class PatchedHTMLTranslator(HTMLTranslator):
    def visit_reference(self, node: Node):
        """Open external links in new tabs.

        `target="_blank"` is inserted for links that are explicitly in the
        "external" class.

        Args:
            node: A docutil reference node.
        """
        if (node.get("newtab")
                or not (node.get("target")
                        or node.get("internal")
                        or "refuri" not in node)):
            node["target"] = "_blank"
        super().visit_reference(node)


def setup(app: Sphinx):
    """Setup the plugin for external links.

    Args:
        app (Sphinx): The whole Sphinx application.
    Returns:
        Plugin metadata.
    """
    app.set_translator("html", PatchedHTMLTranslator)

    return {"parallel_read_safe": True}
