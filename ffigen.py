#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Script to generate FFI bindings from C headers

import io
import sys
import copy

# This is not required if you've installed pycparser and pcpp
# into your site-packages/ with setup.py
sys.path.extend(['.', '..'])

from pycparser import c_parser, c_ast
from pcpp.preprocessor import Preprocessor, OutputDirective, Action

#==============================================================================
# Preprocessing
#==============================================================================
class FFIPreprocessor(Preprocessor):
    def __init__(self, handler, inputs, defines=[], undefines=[], nevers=[], includes=[]):
        super(FFIPreprocessor, self).__init__()

        # Members
        self.handler = handler
        self.inputs = inputs
        self.defines = defines
        self.undefines = undefines
        self.nevers = nevers
        self.includes = includes

        # Override Preprocessor instance variables
        #self.define("__PCPP_VERSION__ " + version)

        # Enable the heuristics which auto apply #pragma once to #include files wholly wrapped in an obvious include guard macro
        self.auto_pragma_once_enabled = True

        # Form of line directive to use, defaults to #line, specify nothing to disable output of line directives
        self.line_directive = '#line'

        # Make output as small as possible
        compress = True
        self.compress = 2 if compress else 0

        # Disable the heuristics which auto apply #pragma once to #include files wholly wrapped in an obvious include guard macro
        self.passthru_unfound_includes = False

        # Unknown macros in expressions cause preprocessor logic to be passed through instead of executed by treating unknown macros as 0L
        self.passthru_undefined_exprs = False

        # Pass through but still execute #defines and #undefs if not always removed by preprocessor logic
        self.passthru_defines = False

        # Pass through comments unmodified
        self.passthru_comments = False

        # Pass through double underscore magic macros unmodified
        passthru_magic_macros = True
        if passthru_magic_macros:
            self.undef('__DATE__')
            self.undef('__TIME__')
            self.expand_linemacro = False
            self.expand_filemacro = False
            self.expand_countermacro = False

        # My own instance variables
        self.bypass_ifpassthru = False
        self.potential_include_guard = None

        # Predefine name as a macro [with value]
        if self.defines:
            self.defines = [x[0] for x in self.defines]
            for d in self.defines:
                if '=' not in d:
                    d += '=1'
                d = d.replace('=', ' ', 1)
                self.define(d)

        # Pre-undefine name as a macro
        if self.undefines:
            self.undefines = [x[0] for x in self.undefines]
            for d in self.undefines:
                self.undef(d)

        # Never define name as a macro, even if defined during the preprocessing.
        if self.nevers:
            self.nevers = [x[0] for x in self.nevers]

        # Path to search for unfound #include's
        if self.includes:
            self.includes = [x[0] for x in self.includes]
            for d in self.includes:
                self.add_path(d)

    def process(self):
        try:
            if len(self.inputs) == 1:
                with open(self.inputs[0]) as f:
                    input = f.read()
                self.parse(input)
            else:
                input = ''
                for i in self.inputs:
                    input += '#include "' + i + '"\n'
                self.parse(input)
            output = io.StringIO()
            self.write(output)
            return output.getvalue()
        except:
            print(traceback.print_exc(10), file = sys.stderr)
            print("\nINTERNAL PREPROCESSOR ERROR AT AROUND %s:%d, FATALLY EXITING NOW\n"
                % (self.lastdirective.source, self.lastdirective.lineno), file = sys.stderr)
            sys.exit(-99)
        return None

    def on_include_not_found(self, is_system_include, curdir, includepath):
        self.handler.on_include_not_found(is_system_include, curdir, includepath)
        if not self.passthru_unfound_includes:
            raise OutputDirective(Action.IgnoreAndRemove)
        return super(FFIPreprocessor, self).on_include_not_found(is_system_include, curdir, includepath)

    def on_unknown_macro_in_defined_expr(self, tok):
        if self.undefines:
            if tok.value in self.undefines:
                return False
        self.handler.on_unknown_macro_in_defined_expr(tok)
        if self.passthru_undefined_exprs:
            return None  # Pass through as expanded as possible
        return super(FFIPreprocessor, self).on_unknown_macro_in_defined_expr(tok)

    def on_unknown_macro_in_expr(self, tok):
        if self.undefines:
            if tok.value in self.undefines:
                return super(FFIPreprocessor, self).on_unknown_macro_in_expr(tok)
        self.handler.on_unknown_macro_in_expr(tok)
        if self.passthru_undefined_exprs:
            return None  # Pass through as expanded as possible
        return super(FFIPreprocessor, self).on_unknown_macro_in_expr(tok)

    def on_directive_handle(self, directive, toks, ifpassthru, precedingtoks):
        self.handler.on_directive_handle(directive, toks, ifpassthru, precedingtoks)
        if ifpassthru:
            if directive.value == 'if' or directive.value == 'elif' or directive == 'else' or directive.value == 'endif':
                self.bypass_ifpassthru = len([tok for tok in toks if tok.value == '__PCPP_ALWAYS_FALSE__' or tok.value == '__PCPP_ALWAYS_TRUE__']) > 0
            if not self.bypass_ifpassthru and (directive.value == 'define' or directive.value == 'undef'):
                if toks[0].value != self.potential_include_guard:
                    raise OutputDirective(Action.IgnoreAndPassThrough)  # Don't execute anything with effects when inside an #if expr with undefined macro
        if (directive.value == 'define' or directive.value == 'undef') and self.nevers:
            if toks[0].value in self.nevers:
                raise OutputDirective(Action.IgnoreAndPassThrough)
        if self.passthru_defines:
            super(FFIPreprocessor, self).on_directive_handle(directive, toks, ifpassthru, precedingtoks)
            return None  # Pass through where possible
        return super(FFIPreprocessor, self).on_directive_handle(directive, toks, ifpassthru, precedingtoks)

    def on_directive_unknown(self, directive, toks, ifpassthru, precedingtoks):
        self.handler.on_directive_unknown(directive, toks, ifpassthru, precedingtoks)
        if ifpassthru:
            return None  # Pass through
        return super(FFIPreprocessor, self).on_directive_unknown(directive, toks, ifpassthru, precedingtoks)

    def on_potential_include_guard(self, macro):
        self.potential_include_guard = macro
        return super(FFIPreprocessor, self).on_potential_include_guard(macro)

    def on_comment(self, tok):
        # Pass through comments unmodified
        if self.passthru_comments:
            return True  # Pass through
        return super(FFIPreprocessor, self).on_comment(tok)

#==============================================================================
# Semantic analysis
#==============================================================================
def explain_c_declaration(c_decl, adapter, expand_struct=False, expand_typedef=False):
    """ Parses the declaration in c_decl and returns a text
        explanation as a string.

        The last external node of the string is used, to allow earlier typedefs
        for used types.

        expand_struct=True will spell out struct definitions recursively.
        expand_typedef=True will expand typedef'd types.
    """
    parser = c_parser.CParser()
    try:
        node = parser.parse(c_decl, filename='<stdin>')
    except c_parser.ParseError:
        e = sys.exc_info()[1]
        return "Parse error:" + str(e)

    if (not isinstance(node, c_ast.FileAST) or
        not isinstance(node.ext[-1], c_ast.Decl)):
        return "Not a valid declaration"

    explanations = io.StringIO()
    for child in node.ext:
        try:
            expanded = expand_element(child, node,
                                             expand_struct=expand_struct,
                                             expand_typedef=expand_typedef)
            explanation = adapter.explain(expanded)
            explanations.write(explanation + "\n")
        except Exception as e:
            raise e
            return "Not a valid declaration: " + str(e)
    return explanations.getvalue()

#---------------------------------------------------------------
# Node traversal
#---------------------------------------------------------------
def expand_element(cdecl, file_ast,
                          expand_struct=False,
                          expand_typedef=False):
    """Expand struct & typedef and return a new expanded node."""
    decl_copy = copy.deepcopy(cdecl)
    _expand_in_place(decl_copy, file_ast, expand_struct, expand_typedef)
    return decl_copy

def _expand_in_place(decl, file_ast, expand_struct=False, expand_typedef=False):
    """
    Recursively expand struct & typedef in place, throw RuntimeError if
    undeclared struct or typedef are used
    """
    typ = type(decl)

    if typ in (c_ast.Decl, c_ast.TypeDecl, c_ast.PtrDecl, c_ast.ArrayDecl):
        decl.type = _expand_in_place(decl.type, file_ast, expand_struct, expand_typedef)
    elif typ == c_ast.Struct:
        if not decl.decls:
            struct = _find_struct(decl.name, file_ast)
            if not struct:
                raise RuntimeError('using undeclared struct %s' % decl.name)
            decl.decls = struct.decls
        for i, mem_decl in enumerate(decl.decls):
            decl.decls[i] = _expand_in_place(mem_decl, file_ast, expand_struct, expand_typedef)
        if not expand_struct:
            decl.decls = []
    elif (typ == c_ast.IdentifierType and decl.names[0] not in ('int', 'char')):
        typedef = _find_typedef(decl.names[0], file_ast)
        if not typedef:
            raise RuntimeError('using undeclared type %s' % decl.names[0])
        if expand_typedef:
            return typedef.type
    return decl

def _find_struct(name, file_ast):
    """Receives a struct name and return declared struct object in file_ast """
    for node in file_ast.ext:
        if (type(node) == c_ast.Decl and
           type(node.type) == c_ast.Struct and
           node.type.name == name):
            return node.type

def _find_typedef(name, file_ast):
    """Receives a type name and return typedef object in file_ast """
    for node in file_ast.ext:
        if type(node) == c_ast.Typedef and node.name == name:
            return node

#---------------------------------------------------------------
# Node explanation
#---------------------------------------------------------------
class NLAdapter:
    def explain(self, node):
        return self._explain_decl_node(node)

    def _explain_decl_node(self, decl_node):
        """ Receives a c_ast.Decl note and returns its explanation in English. """
        storage = ' '.join(decl_node.storage) + ' ' if decl_node.storage else ''
        return (decl_node.name + " is a " + storage + self._explain_type(decl_node.type))

    def _explain_type(self, decl):
        """ Recursively explains a type decl node """
        typ = type(decl)

        if typ == c_ast.TypeDecl:
            quals = ' '.join(decl.quals) + ' ' if decl.quals else ''
            return quals + self._explain_type(decl.type)
        elif typ == c_ast.Typename or typ == c_ast.Decl:
            return self._explain_type(decl.type)
        elif typ == c_ast.IdentifierType:
            return ' '.join(decl.names)
        elif typ == c_ast.PtrDecl:
            quals = ' '.join(decl.quals) + ' ' if decl.quals else ''
            return quals + 'pointer to ' + self._explain_type(decl.type)
        elif typ == c_ast.ArrayDecl:
            arr = 'array'
            if decl.dim: arr += '[%s]' % decl.dim.value
            return arr + " of " + self._explain_type(decl.type)
        elif typ == c_ast.FuncDecl:
            if decl.args:
                params = [self._explain_type(param) for param in decl.args.params]
                args = ', '.join(params)
            else:
                args = ''
            return ('function(%s) returning ' % (args) +
                    self._explain_type(decl.type))
        elif typ == c_ast.Struct:
            decls = [self._explain_decl_node(mem_decl) for mem_decl in decl.decls] if decl.decls else []
            members = ', '.join(decls)
            return ('struct%s ' % (' ' + decl.name if decl.name else '') +
                    ('containing {%s}' % members if members else ''))

#---------------------------------------------------------------
# Scheme
#---------------------------------------------------------------
class SchemeAdapter:
    def __init__(self):
        self.fn_typedefs = []

    def explain(self, node):
        return self._explain_decl_node(node)

    def _explain_decl_node(self, decl_node):
        decl = decl_node.type
        typ = type(decl)

        if [i for i in ['struct', 'typedef'] if i in decl_node.storage]:
            if type(decl.type) == c_ast.FuncDecl and 'typedef' in decl_node.storage:
                self.fn_typedefs.append(decl_node.name)
            return self.spit_ftype(decl_node.name, self._explain_type(decl.type))
        elif typ == c_ast.FuncDecl:
            params = [self._explain_type(param) for param in decl.args.params] if decl.args else []
            params = [] if len(params) == 1 and params[0] == 'void' else params
            rtype = self._explain_type(decl.type)
            return self.spit_foreign_procedure(decl_node.name, params, rtype)

        return 'TODO: ' + ' '.join(decl_node.storage) + ' ' + str(typ)

    def spit_foreign_procedure(self, name, params, rtype):
        return """(define {fn} (foreign-procedure "{fn}" ({param_types}) {rval_type}))""" \
                .format(fn=name, param_types=' '.join(params), rval_type=rtype)

    def spit_ftype(self, name, ttype):
        return """(define-ftype {name} {ttype})""".format(name=name, ttype=ttype)

    def transform_param(self, param, proxy_types=True):
        mappings = {
            'unsigned char': 'char',
            'unsigned short': 'unsigned-short',
            'unsigned int': 'unsigned-int',
            'unsigned long': 'unsigned-long',
            'unsigned long long': 'unsigned-long-long',
            'long long': 'long-long',
        }
        if proxy_types:
            proxy_mappings = {
                '(* const char)': 'string',
                '(* char)': 'string',
            }
            mappings.update(proxy_mappings)
        return mappings[param] if param in mappings else param

    def _explain_type(self, decl, proxy_types=True):
        """ Recursively explains a type decl node """
        typ = type(decl)

        if typ == c_ast.TypeDecl:
            tp = self._explain_type(decl.type, proxy_types)
            return self.transform_param(tp, proxy_types)
        elif typ == c_ast.Typename or typ == c_ast.Decl:
            return self._explain_type(decl.type, proxy_types)
        elif typ == c_ast.IdentifierType:
            identifier = ' '.join(decl.names)
            return identifier if not identifier in self.fn_typedefs else '(* {})'.format(identifier)
        elif typ == c_ast.PtrDecl:
            ptype = self._explain_type(decl.type, proxy_types)
            if ptype == 'void': # Possibly other primitive types?
                ptr = '{}*'.format(ptype)
            else:
                ptr = '(* {})'.format(ptype)
            return ptr if type(decl.type) != c_ast.PtrDecl else 'uptr'
        elif typ == c_ast.ArrayDecl:
            arr = '(array {} {})'.format(decl.dim.value, self._explain_type(decl.type, proxy_types))
            return arr
        elif typ == c_ast.FuncDecl:
            params = [self._explain_type(param, proxy_types) for param in decl.args.params]
            params = [] if len(params) == 1 and params[0] == 'void' else params
            args = ' '.join(params) if decl.args else ''
            return '(function ({}) {})'.format(args, self._explain_type(decl.type, proxy_types))
        elif typ == c_ast.Struct:
            decls = ['[{} {}]'.format(mem_decl.name, self._explain_type(mem_decl.type, False)) \
                        for mem_decl in decl.decls] if decl.decls else []
            members = ' '.join(decls)
            return '(struct{})'.format((' ' + members if members else ''))

#==============================================================================
# Handler
#==============================================================================
class Handler():
    def __init__(self):
        pass

    def on_include_not_found(self, is_system_include, curdir, includepath):
        #print('Include not found: ' + includepath)
        pass

    def on_unknown_macro_in_defined_expr(self, tok):
        #print('Unknown macro in defined expr: ' + tok.type + " = " + tok.value)
        pass

    def on_unknown_macro_in_expr(self, tok):
        #print('Unknown macro in expr: ' + tok.type + " = " + tok.value)
        pass

    def on_directive_handle(self, directive, toks, ifpassthru, precedingtoks):
        #if (directive.value == 'define'):
        #    for tok in toks:
        #        print(tok.type + " = " + tok.value)
        pass

    def on_directive_unknown(self, directive, toks, ifpassthru, precedingtoks):
        #print('Unknown directive: ' + directive.value)
        pass

#==============================================================================
# Entrypoint
#==============================================================================
def preprocess(c_src, handler):
    p = FFIPreprocessor(handler, [c_src])
    r = p.process()
    return r

def analyse(c_decl, hander):
    # Sample input
    # c_decl = "char *(*(**foo[][8])())[];"
    #a = NLAdapter()
    a = SchemeAdapter()
    r = explain_c_declaration(c_decl, a)
    return r

def main():
    h = Handler()
    r = preprocess(sys.argv[1], h)
    r = analyse(r, h)
    print(r)

if __name__ == "__main__":
    main()
