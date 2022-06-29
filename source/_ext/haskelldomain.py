#!/usr/bin/env python3
"""
Haskell domain for Sphinx.
"""

import re
from typing import Any, Dict, Iterator, List, NamedTuple, Tuple, cast

from docutils import nodes
from docutils.parsers.rst import Directive, directives
from sphinx.addnodes import (desc_addname, desc_name, desc_sig_keyword,
                             desc_sig_operator, desc_sig_punctuation,
                             desc_sig_space, desc_signature, desc_type, index)
from sphinx.application import Sphinx
from sphinx.directives import ObjectDescription
from sphinx.domains import Domain, Index, ObjType
from sphinx.locale import _, __
from sphinx.roles import XRefRole
from sphinx.util.nodes import make_refnode

__all__ = ["HaskellModule", "HaskellCurrentModule", "HaskellObject",
           "HaskellXRefRole", "HaskellDomain", "HaskellModuleIndex",
           "setup"]


func_pre = re.compile(r"^\s*(\S+)\s+::\s+(?:\(?(.+?)\)?\s+=>\s+)?(.+)")

ObjectEntry = NamedTuple(
    "ObjectEntry", [("docname", str), ("node_id", str), ("objtype", str)]
)


class HaskellModule(Directive):
    """Description of a Haskell module."""

    has_content: bool = False
    required_arguments: int = 1
    option_spec = {
        "platform": lambda x: x,
        "synopsis": lambda x: x,
        "noindex": directives.flag,
        "deprecated": directives.flag,
    }

    def run(self) -> List:
        env = self.state.document.settings.env
        modname = self.arguments[0].strip()
        noindex = "noindex" in self.options
        env.temp_data["hs:module"] = modname
        env.domaindata["hs"]["modules"][modname] = (
            env.docname,
            self.options.get("synopsis", ""),
            self.options.get("platform", ""),
            "deprecated" in self.options,
        )
        target_node = nodes.target(
            "", "", ids=["module-" + modname], ismod=True)
        self.state.document.note_explicit_target(target_node)
        ret = [target_node]
        if "platform" in self.options:
            platform = self.options["platform"]
            pl_node = nodes.paragraph()
            pl_node += nodes.emphasis("", _("Platform: "))
            pl_node += nodes.Text(platform, platform)
            ret.append(pl_node)
        # synopsis is only used in the modindex
        if not noindex:
            indextext = f"{modname} ({_('module')})"
            inode = index(
                entries=[
                    ("single", indextext, "module-" + modname, modname, None)
                ]
            )
            ret.append(inode)
        return ret


class HaskellCurrentModule(Directive):
    """Documenting symbols in the current module without linking."""

    has_content: bool = False
    required_arguments: int = 1
    final_argument_whitespace: bool = False
    option_spec: Dict = {}

    def run(self) -> List:
        env = self.state.document.settings.env
        modname = self.arguments[0].strip()
        if modname == "None":
            env.temp_data["hs:module"] = None
        else:
            env.temp_data["hs:module"] = modname
        return []


class HaskellObject(ObjectDescription):
    """A Haskell object."""

    option_spec: Dict = {
        "module": directives.unchanged,
    }

    def _parse_func_spec(self, sig: str):
        """Parse haskell function declaration."""
        m = func_pre.match(sig)
        if m is None:
            raise Exception("No function definition is found.")
        name, constraints, spec = m.groups()
        name, spec = name.strip(), spec.strip()
        constraints = constraints.strip() if constraints else constraints
        return name, constraints, spec

    def _resolve_module_name(
            self, signode: desc_signature, modname: str, name: str) -> str:
        """Find the module name for the object.

        Args:
            signode (desc_signature): The signature node.
            modname (str): The module name.
            name (str): The object name.
        Returns:
            The qualified name of the object.
        """
        env_modname = self.options.get(
            "module", self.env.temp_data.get("hs:module", "")
        )
        if modname:
            fullname = modname + "." + name
            signode["module"] = modname
        else:  # try to fetch module name for the environment
            fullname = (env_modname + "." + name
                        if env_modname
                        else name)
            signode["module"] = env_modname if env_modname else None
        return fullname

    def handle_signature(self, sig: str, signode: desc_signature) -> str:
        """Parse object signatures into nodes.

        Args:
            sig (str): The string representing a signature.
            signode (desc_signature): The node representing a signature.
        Returns:
            The value that is used in creating the index.
        """
        space = desc_sig_space(sig, " ")
        name, constraints, spec = self._parse_func_spec(sig)
        # with a module name?
        modname = ""
        if not name.startswith("(") and not name.endswith(")"):
            # not an operator
            if len(name.split(".")) > 1:  # with a module name
                modname, name = name.rsplit(".", 1)
        # fullname
        fullname = self._resolve_module_name(signode, modname, name)
        # module name if any
        if signode.get("module"):
            signode.append(desc_addname(sig, signode["module"] + "."))
        # function name
        signode.append(desc_name(sig, name))
        signode.append(space)
        signode.append(desc_sig_punctuation(sig, "::"))
        signode.append(space)
        # type constraints
        if constraints:
            cstr_list = [c.strip() for c in constraints.split(",")]
            cstr_list_len = len(cstr_list)
            if cstr_list_len > 1:
                signode.append(desc_sig_punctuation(sig, "("))
            for i in range(cstr_list_len):
                tclass, tparam = cstr_list[i].split()
                signode.append(desc_sig_keyword(sig, tclass))
                signode.append(space)
                signode.append(desc_type(sig, tparam))
                if i != cstr_list_len - 1:  # not the last one
                    signode.append(desc_sig_punctuation(sig, ","))
                    signode.append(space)
            if cstr_list_len > 1:
                signode.append(desc_sig_punctuation(sig, ")"))
            signode.append(space)
            signode.append(desc_sig_operator(sig, "=>"))
            signode.append(space)
        # specification
        plist = [p.strip() for p in spec.split("->")]
        plist_len = len(plist)
        for i in range(plist_len):
            p = plist[i]
            if p.startswith("[") and p.endswith("]"):  # a list
                signode.append(desc_sig_keyword(sig, "["))
                signode.append(desc_type(sig, p.strip("[]")))
                signode.append(desc_sig_keyword(sig, "]"))
            elif len(p.split()) > 1:  # an algebraic data
                constructor, *tparams = p.split()
                signode.append(desc_sig_keyword(sig, constructor))
                for tparam in tparams:
                    signode.append(space)
                    signode.append(desc_type(sig, tparam))
            else:
                signode.append(desc_type(sig, p))
            if i != plist_len - 1:  # not the last one
                signode.append(space)
                signode.append(desc_sig_operator(sig, "->"))
                signode.append(space)

        return fullname

    def add_target_and_index(
            self, name: str, sig: str, signode: desc_signature):
        """Add reference IDs and entries to self.indexnode.

        Args:
            name (str): What handle_signature() returns.
            sig (str): The signature text.
            signode (desc_signature): The node representing a signature.
        """
        # node_id = make_id(self.env, self.state.document, "", name)
        node_id = name  # preserve special characters
        signode["ids"].append(node_id)
        self.state.document.note_explicit_target(signode)
        domain = cast(HaskellDomain, self.env.get_domain("hs"))
        domain.note_object(name, self.objtype, node_id, location=signode)
        indextext = f"{name} (Haskell {_(self.objtype)})"
        if indextext:
            self.indexnode["entries"].append(
                ("single", indextext, name, "", None)
            )


class HaskellXRefRole(XRefRole):
    """The reference role for Haskell symbols."""

    def process_link(self, env, refnode, has_explicit_title, title, target):
        """How the reference link is created and displayed."""
        mod = env.temp_data.get("hs:module")
        refnode["hs:module"] = mod
        if not has_explicit_title:
            target = target.lstrip("~")  # only has a meaning for a title
            # don't display the module if the first character is a tilde
            if title[0:1] == "~":
                title = title[1:]
                dot = title.rfind(".")
                if dot != -1:
                    title = title[dot + 1:]
        return title, target


class HaskellModuleIndex(Index):
    name = "modindex"
    localname = "Haskell " + _("Module Index")
    shortname = "Haskell " + _("Modules")

    def generate(self, docnames=None):
        content = {}
        ignores = self.domain.env.config["modindex_common_prefix"]
        ignores = sorted(ignores, key=len, reverse=True)
        # all modules sorted by name
        modules = sorted(
            self.domain.data["modules"].items(), key=lambda x: x[0].lower()
        )
        # sort out collapsible modules
        prev_modname = ""
        num_toplevels = 0
        for modname, (docname, synopsis, platforms, deprecated) in modules:
            if docnames and docname not in docnames:
                continue
            for ignore in ignores:
                if modname.startswith(ignore):
                    modname = modname[len(ignore):]
                    stripped = ignore
                    break
            else:
                stripped = ""
            if not modname:
                # stripped the whole module name?
                modname, stripped = stripped, ""
            entries = content.setdefault(modname[0].lower(), [])
            module = modname.split(".")[0]
            if module != modname:
                # submodule
                if prev_modname == module:
                    # first submodule
                    entries[-1][1] = 1
                elif not prev_modname.startswith(module):
                    # submodule without a parent in list, add a dummy entry
                    entries.append([stripped + module, 1, "", "", "", "", ""])
                subtype = 2
            else:
                num_toplevels += 1
                subtype = 0
            qualifier = deprecated and _("Deprecated") or ""
            entries.append(
                [
                    stripped + modname,
                    subtype,
                    docname,
                    "module-" + stripped + modname,
                    platforms,
                    qualifier,
                    synopsis,
                ],
            )
            prev_modname = modname
        # only collapse if number of toplevel modules is larger than
        # number of submodules.
        collapse = len(modules) - num_toplevels < num_toplevels
        # sort by first letter
        content = sorted(content.items())
        return content, collapse


class HaskellDomain(Domain):
    """Haskell language domain."""

    name = "hs"
    label = "Haskell"
    object_types = {
        "function": ObjType(_("function"), "func"),
        "module": ObjType(_("module"), "mod"),
    }
    directives = {
        "currentmodule": HaskellCurrentModule,
        "function": HaskellObject,
        "module": HaskellModule,
        "object": HaskellObject,
    }
    roles = {
        "func": HaskellXRefRole(),
        "mod": HaskellXRefRole(),
        "ref": HaskellXRefRole(),
    }
    initial_data = {
        "functions": {},
        "modules": {},
        "objects": {},
    }
    indices = [HaskellModuleIndex]

    def clear_doc(self, docname: str):
        for fullname, obj in list(self.objects.items()):
            if obj.docname == docname:
                del self.objects[fullname]

    def _find_obj(self, env, modname, name, objtype, searchorder=0):
        """Find a Haskell object for 'name'."""
        obj = self.objects.get(name, None)
        if obj is None:
            fqn = f"{modname}.{name}"
            obj = self.objects.get(fqn)
            if obj is not None:
                name = fqn
        if obj:
            return name, obj.docname
        return None, None

    def get_objects(self) -> Iterator[Tuple[str, str, str, str, str, int]]:
        for refname, obj in self.objects.items():
            yield refname, refname, obj.objtype, obj.docname, obj.node_id, 1

    def resolve_xref(
            self, env, fromdocname, builder, typ, target, node, contnode):
        modname = node.get("hs:module")
        searchorder = node.hasattr("refspecific") and 1 or 0
        name, obj = self._find_obj(env, modname, target, typ, searchorder)
        if not obj:
            return None
        else:
            return make_refnode(
                builder, fromdocname, obj, name, contnode, name)

    @property
    def objects(self) -> Dict[str, ObjectEntry]:
        return self.data.setdefault("objects", {})

    def note_object(
            self, name: str, objtype: str, node_id: str, location: Any = None):
        """Note an object for cross-reference."""
        if name in self.objects:
            other = self.objects[name]
            self.state_machine.reporter.warning(
                __("duplicate object description of %s, "
                   "other instance in %s, use :noindex: for on of them"),
                name,
                other.docname,
            )
        self.objects[name] = ObjectEntry(self.env.docname, node_id, objtype)


def setup(app: Sphinx) -> Dict:
    """Setup the plugin.

    Args:
        app (Sphinx): The whole Sphinx application.
    Returns:
        Plugin metadata.
    """
    app.add_domain(HaskellDomain)

    return {"parallel_read_safe": True}
