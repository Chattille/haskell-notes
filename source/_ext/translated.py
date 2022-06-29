#!/usr/bin/env python3
"""
An interpreted role for showing the source text of a translated phrase.

Usage:

    :tr:`Target text (Source text)`

Example:

    :tr:`艾伦·图灵 (Alan Turing)`

    will become:

    <span class="translated" data="Alan Turing">艾伦·图灵</span>
"""

from typing import Dict, List, Tuple

from docutils.nodes import Element, Inline, Node
from docutils.parsers.rst.states import Inliner
from sphinx.application import Sphinx
from sphinx.writers.html import HTMLTranslator

__all__ = ["TranslatedNode", "visit_translated_node",
           "translated_role", "setup"]


class TranslatedNode(Element, Inline):
    def __init__(
            self, rawsource: str = '', tt: str = '', st: str = '',
            *children, **attributes):
        """A node representing a translated text.

        Args:
            rawsource (str): The whole interpreted text.
            tt (str): The target text.
            st (str): The source text, inside a parentheses.
        """
        super().__init__(rawsource, *children, **attributes)
        self.set_class("translated")
        self.target_text = tt
        self.source_text = st


def visit_translated_node(translator: HTMLTranslator, node: TranslatedNode):
    """Transform TranslatedNode to HTML tags.

    Args:
        translator (HTMLTranslator): Sphinx HTML translator.
        node (TranslatedNode): The TranslatedNode being visited.
    """
    # source text is put in the "data" attribute
    opener = translator.starttag(node, "span", '', data=node.source_text)
    translator.body.append(opener)
    translator.body.append(node.target_text)


def translated_role(
        name: str, rawtext: str, text: str, lineno: int,
        inliner: Inliner, options: Dict = {},
        content: List[str] = []) -> Tuple[List[Node], List[str]]:
    """A role processor for a translated text.

    Args:
        name (str): The role name used in the document.
        rawtext (str): The whole markup snippet.
        text (str): The text marked with the role.
        lineno (int): The line number where `rawtext` appears in the input.
        inliner (Inliner): The inline processor that calls this function.
        options (Dict): A dictionary of directive options for customization.
        content (List[str]): A list of strings,
                             the directive content for customization.
    Returns:
        A list of docutils nodes, and a list of system messages.
    """
    try:
        tt, st = text.split("(", 1)
    except ValueError:  # no '(source text)' provided
        msg = inliner.reporter.error(
            "Must specify the source text.", line=lineno)
        prb = inliner.problematic(rawtext, rawtext, msg)
        return [prb], [msg]

    tt = tt.strip()
    st = st.strip().rstrip(")")
    translated_node = TranslatedNode(rawtext, tt, st, **options)
    return [translated_node], []


def setup(app: Sphinx) -> Dict:
    """Setup translated roles.

    Args:
        app (Sphinx): The whole Sphinx application.
    Returns:
        Plugin metadata.
    """
    # latex not supported
    app.add_node(
        TranslatedNode,
        html=(visit_translated_node, HTMLTranslator.depart_inline),
        text=(visit_translated_node, HTMLTranslator.depart_inline),
        latex=(lambda *args, **kwargs: None, lambda *args, **kwargs: None))
    app.add_role('tr', translated_role)

    return {"parallel_read_safe": True}
