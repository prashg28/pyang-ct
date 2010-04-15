# Copyright (c) 2010, Nokia Siemens Networks Oy
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#    * Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#    * Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in
#      the documentation and/or other materials provided with the
#      distribution.
#    * Neither the name of the Nokia Siemens Networks Oy nor the names of its
#      contributors may be used to endorse or promote products derived
#      from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
# TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

"""Extending YANG with Language Abstractions draft-linowski-netmod-yang-abstract-01 plugin"""
import copy
import optparse
import sys
from pyang import plugin
from pyang import grammar
from pyang import statements
from pyang import error
from pyang.error import err_add
from pyang .util import attrsearch
from pyang import syntax
from pyang import util


ct_module_name = 'complex-types'
str_complex_type = 'complex-type'
str_extends = 'extends'
str_abstract = 'abstract'
str_instance = 'instance'
str_instance_list = 'instance-list'
str_instance_type = 'instance-type'

class ComplexTypesPlugin(plugin.PyangPlugin):
    def add_opts(self, optparser):
        optlist = [optparse.make_option("--enable-complex-types",
                             dest="is_ct_enabled",
                             action="store_true",
                             help="Enable complex type extension")]
        g = optparser.add_option_group("YANG Language Abstractions")
        g.add_options(optlist)

def pyang_plugin_init():
    """Called by pyang plugin framework at to initialize the plugin."""
    # Register the plugin
    plugin.register_plugin(ComplexTypesPlugin())

    # check whether the plugin is enabled
    try:
        sys.argv.index("--enable-complex-types")
    except ValueError:
        return

   
    # Register that we handle extensions from the YANG module 'complex-types' 
    grammar.register_extension_module(ct_module_name)

    # Register the special grammar
    for (stmt, occurance, (arg, rules), add_to_stmts) in complex_types_stmts:
        grammar.add_stmt((ct_module_name, stmt), (arg, rules))
        grammar.add_to_stmts_rules(add_to_stmts,
                                   [((ct_module_name, stmt), occurance)])

    # Add validation steps
    # init phase: initalizes the module/submodule statements
    statements.add_validation_phase('init_complex_type', after='init2')
    # grammar phase:
    # verifies that all complex-types are unique within a parent node
    statements.add_validation_phase('post_grammar_complex_type',
                                        after='grammar')
    # include phase: loads complex-types from a submodule and
    # checks for complex-type collisions in a submodule
    statements.add_validation_phase('include_complex_type', after='import')
    # validate typed instance identifiers
    statements.add_validation_phase('instance_type', after='type')
    # expand the 'extends' statements;
    statements.add_validation_phase('instantiate_extends', after='expand_1')
    # instantiate all 'instance'/'instance-list' statements;
    # this phase should be marked as v_i_children (validate i_children
    # instead of substatemets) so that we could expand the 'instance'/
    # 'instance-list' created by augmentation nested into a 'uses' statement;
    statements.add_validation_phase('instantiate_instance',
                                        after='instantiate_extends')
    statements.set_phase_i_children('instantiate_instance')
    # inherit 'config' properties for statements nested into 'complex-type';
    # set 'i_config' values;
    statements.add_validation_phase('inherit_properties_complex_type',
                                        after='inherit_properties')
    # validate augmentation statements that refer to 'instace'/'instance-list'
    statements.add_validation_phase('pre_expand_2', before='expand_2')
    # unique names phase: checks for complex type collisions in submodules
    statements.add_validation_phase('unique_name_complex_type',
                                        before='unique_name')
    # expand all recursive datastructures reffered by deviation
    statements.add_validation_phase('pre_reference_2', before='reference_2')

    # Add functions to the validation map
    statements.add_validation_fun('init_complex_type',
                                  ['*'],
                                  v_init_complex_type)

    statements.add_validation_fun('post_grammar_complex_type',
                                  ['*'],
                                  v_post_grammar_complex_type)

    statements.add_validation_fun('include_complex_type',
                                  ['module', 'submodule'],
                                  v_include_complex_type)

    statements.add_validation_fun('type',
                                  [(ct_module_name, str_complex_type)],
                                  v_type_complex_type)

    statements.add_validation_fun('type',
                                  [(ct_module_name, str_instance),
                                  (ct_module_name, str_instance_list)],
                                  v_type_instance)

    statements.add_validation_fun('instance_type',
                                  [(ct_module_name, str_instance_type)],
                                  v_type_instance_type)

    statements.add_validation_fun('instantiate_extends',
                                  [(ct_module_name, str_complex_type)],
                                  v_instantiate_extends)

    statements.add_validation_fun('instantiate_instance',
                                  [(ct_module_name, str_instance),
                                  (ct_module_name, str_instance_list)],
                                  v_instantiate_instance)

    statements.add_validation_fun('inherit_properties_complex_type',
                                  [(ct_module_name, str_complex_type)],
                                  v_inherit_properties_complex_type)

    statements.add_validation_fun('pre_expand_2',
                                  ['augment'],
                                  v_pre_expand_2_augment)

    statements.add_validation_fun('unique_name_complex_type',
                                  ['module'],
                                  v_unique_name_complex_type)

    statements.add_validation_fun('pre_reference_2',
                                  ['deviation'],
                                  v_reference_load_recursive_nodes)

    statements.add_validation_fun('reference_1',
                                  [(ct_module_name, str_complex_type)],
                                  v_reference_complex_type)

    statements.add_validation_fun('reference_2',
                                  [(ct_module_name, str_instance_list)],
                                  v_reference_instance_list)

    statements.add_validation_fun('unused',
                                  [(ct_module_name, str_complex_type)],
                                  v_unused_complex_type)

    # Add 'complex-type'/'instance'/'instance-list' to keywords with children
    statements.add_keyword_with_children((ct_module_name, str_complex_type))
    statements.add_keyword_with_children((ct_module_name, str_instance))
    statements.add_keyword_with_children((ct_module_name, str_instance_list))

    # Add 'instance' and 'instance-list' to the data keywords
    statements.add_data_keyword((ct_module_name, str_instance))
    statements.add_data_keyword((ct_module_name, str_instance_list))

    # add 'instance'/'instance-list' to the possible refinements
    statements.add_refinement_element('description',
                                        (ct_module_name, str_instance))
    statements.add_refinement_element('description',
                                        (ct_module_name, str_instance_list))
    statements.add_refinement_element('reference',
                                        (ct_module_name, str_instance))
    statements.add_refinement_element('reference',
                                        (ct_module_name, str_instance_list))
    statements.add_refinement_element('config', (ct_module_name, str_instance))
    statements.add_refinement_element('config',
                                        (ct_module_name, str_instance_list))
    statements.add_refinement_element('must', (ct_module_name, str_instance))
    statements.add_refinement_element('must',
                                        (ct_module_name, str_instance_list))
    statements.add_refinement_element('mandatory',
                                        (ct_module_name, str_instance))
    statements.add_refinement_element('min-elements',
                                        (ct_module_name, str_instance_list))
    statements.add_refinement_element('max-elements',
                                        (ct_module_name, str_instance_list))

    # add possibale deviation types for 'instance'/'instance-list' statements
    statements.add_deviation_element('config', (ct_module_name, str_instance))
    statements.add_deviation_element('config',
                                        (ct_module_name, str_instance_list))
    statements.add_deviation_element('must', (ct_module_name, str_instance))
    statements.add_deviation_element('must',
                                        (ct_module_name, str_instance_list))
    statements.add_deviation_element('mandatory',
                                        (ct_module_name, str_instance))
    statements.add_deviation_element('min-elements',
                                        (ct_module_name, str_instance_list))
    statements.add_deviation_element('max-elements',
                                        (ct_module_name, str_instance_list))

    # define additional error messages:
    error.add_error_code('COMPLEX_TYPE_ALREADY_DEFINED', 1,
        'complex-type "%s" is already defined at %s')
    error.add_error_code('COMPLEX_TYPE_NOT_FOUND', 1,
        'complex type "%s" not found in module "%s"')
    error.add_error_code('ABSTRACT_NOT_ALLOWED', 1,
        'base complex type "%s" of abstract type "%s" must be abstract')
    error.add_error_code('ABSTRACT_NOT_INSTANTIATED', 1,
        'abstract complex type "%s" can not be instantiated')
    error.add_error_code('UNUSED_COMPLEX_TYPE', 4,
        'complex type "%s" not used')
    error.add_error_code('REDEFINED_KEY', 1,
        'key for complex type "%s" is already defined in base type "%s" at %s')
    error.add_error_code('KEY_REQUIRED', 1,
        'complex-type "%s" instantiated at %s with config true must have key')
    error.add_error_code('REFINE_NOT_INHERITED', 1,
        '"refine" can not be applied to non-inherited node "%s" defined at %s')
    error.add_error_code('BAD_REF_AUG', 1,
        'refinement and augmentation of instance "%s" at %s is not allowed')
    error.add_error_code('MANDATORY_AUGMENTATION', 1,
        'the node "%s" at %s added by the augmentation must not be mandatory')
    error.add_error_code('INSTANCE_IDENTIFIER_REQUIRED', 1,
        'instance-type may not be specified for "%s" type')

def v_init_complex_type(ctx, stmt):
    """Initializes i_complex_type"""
    stmt.i_complex_types = {}

def v_post_grammar_complex_type(ctx, stmt):
    """Verifies that all complex-types are unique within a parent node
    Called for every statement.
    Stores all complex-types in stmt.i_complex_types
    """
    defs = [((ct_module_name, str_complex_type), 
        'COMPLEX_TYPE_ALREADY_DEFINED', stmt.i_complex_types)]
    
    for (keyword, errcode, dict) in defs:
        for definition in stmt.search(keyword):
            if definition.arg in dict:
                other = dict[definition.arg]
                err_add(ctx.errors, definition.pos,
                        errcode, (definition.arg, other.pos))
            else:
                dict[definition.arg] = definition

def v_include_complex_type(ctx, stmt):
    """Loads complex-types from a submodule.
    Checks for complex-type collisions in a submodule.
    """
    includes = stmt.search('include')

    def get_module(i):
        # check if the module to import is already added
        modulename = i.arg
        r = i.search_one('revision-date')
        rev = None
        if r is not None:
            rev = r.arg
        # try to add the module to the context
        module = ctx.search_module(i.pos, modulename, rev)
        return module

    for i in includes:
        submodule = get_module(i)
        if submodule is not None and submodule.keyword != 'submodule':
            return

        if submodule is not None:
            # verify that the submodule's complex-type definitions do not
            # collide with the module's complex-type definitions
            defs = \
                [(submodule.i_complex_types, stmt.i_complex_types,
                  'COMPLEX_TYPE_ALREADY_DEFINED')]
            for (subdict, dict, errcode) in defs:
                for name in subdict:
                    subdefinition = subdict[name]
                    if name in dict:
                        other = dict[name]
                        if other != subdefinition:
                            err_add(ctx.errors, other.pos,
                                    errcode, (name, subdefinition.pos))
                    else:
                        dict[name] = subdefinition

def v_type_complex_type(ctx, stmt):
    """ Checks complex types for circular dependency, collisions of names,
    'extends' and 'abstract' statements.
    """
    if hasattr(stmt, 'i_is_validated'):
        if stmt.i_is_validated == True:
            # this complex-type has already been validated
            return
        elif stmt.i_is_circular == True:
            return
        elif stmt.i_is_validated == 'in_progress':
            err_add(ctx.errors, stmt.pos,
                    'CIRCULAR_DEPENDENCY', ('type', stmt.arg) )
            stmt.i_is_circular = True
            return

    # 'True' - when the complex type is the part of a circular chain
    stmt.i_is_circular = False
    stmt.i_is_validated = 'in_progress'
    # 'True' - if there is no reference to the type
    stmt.i_is_unused = True
    stmt.i_has_i_children = True
    stmt.i_base_type = None

    name = stmt.arg

    # check if a complex type with the same name has not been already defined
    if stmt.parent.parent is not None:
        # non-top-level complex-type;
        # start from stmt.parent.parent, because stmt.parent has been checked 
        ctype = search_complex_type(stmt.parent.parent, name)
        if ctype is not None:
             err_add(ctx.errors, stmt.pos, 
                'COMPLEX_TYPE_ALREADY_DEFINED', (name, ctype.pos))

    # check the 'extends' substatement if present
    extends = stmt.search_one((ct_module_name, str_extends))
    if extends is not None:
        v_type_extends(ctx, extends)

    # check the 'abstract' substatement if present
    abstract = stmt.search_one((ct_module_name, str_abstract))
    if abstract is not None:
        v_type_abstract(ctx, abstract)
    else:
        stmt.i_is_abstract = False
        
    stmt.i_is_validated = True

def v_type_extends(ctx, stmt):
    """Checks whether the extended class exists"""
    check_complex_type_reference(ctx, stmt)
    stmt.parent.i_base_type = stmt.i_complex_type

def check_complex_type_reference(ctx, stmt):
    """Checks the reference to a complex type"""
    stmt.i_complex_type = None
    (prefix, name) = get_identifier_ref(stmt.arg)
    resolve_complex_type(ctx, prefix, name, stmt)
    validate_complex_type(ctx, stmt)

    # check whether the instantiated complex type is not abstract
    if not stmt.i_complex_type is None:
        stmt.i_complex_type.i_unused = False

def v_type_abstract(ctx, stmt):
    """ Check the base complex type, if present, is also abstract."""
    if (stmt.arg == 'true'):
        stmt.parent.i_is_abstract = True
        # check whether the complex type extends another one
        if stmt.parent.i_base_type is not None:
            if not stmt.parent.i_base_type.i_is_abstract:
                err_add(ctx.errors, stmt.pos, 'ABSTRACT_NOT_ALLOWED',
                    (stmt.parent.i_base_type.arg, stmt.parent.arg))
    else:
        stmt.parent.i_is_abstract = False                

def v_type_instance(ctx, stmt):
    """Checks whether the instance refers to an existing complex type.
    The complex type must not be abstract.
    """
    stmt.i_complex_type = None
    # find the 'type'
    type = stmt.search_one((ct_module_name, str_instance_type))
    if type is None or type.is_grammatically_valid == False:
        # error is already reported by grammar check
        return

    check_complex_type_reference(ctx, type)
    stmt.i_complex_type = type.i_complex_type

def v_type_instance_type(ctx, stmt):
    """Validate typed instance identifiers"""
    if stmt.parent.keyword == 'type':
        if not stmt.parent.arg == 'instance-identifier':
            err_add(ctx.errors, stmt.pos, 'INSTANCE_IDENTIFIER_REQUIRED',
                (stmt.parent.arg))
        check_complex_type_reference(ctx, stmt)

def v_instantiate_extends(ctx, stmt):
    """Instantiate 'extends' statements, expand the complex type with
    the nodes from the base complex type. Applies refinements.
    """
    # the complex type is not grammatically valid
    if hasattr(stmt, 'is_grammatically_valid') and \
            not stmt.is_grammatically_valid:
        return
    # there is a circular dependency, don't process the 'extends' statement
    if stmt.i_is_circular:
        return
    bt = stmt.i_base_type
    # the complex type does not extend any other class
    if bt is None:
        return
    if hasattr(stmt, 'i_base_type_expanded'):
        return
    extends = stmt.search_one((ct_module_name, str_extends))
    v_instantiate_extends(ctx, bt)
    # copy the base complex type definitions into stmt.i_children
    names = {}
    for ch in bt.i_children:
        # don't copy the type since it cannot be modified anyway.
        if not (ch.i_module.i_modulename, ch.arg) in names:
            newnode = ch.copy(stmt, extends,
                      nocopy=['type','uses','unique','typedef','grouping'],
                      copyf=post_copy_fnc(extends, True))       
            stmt.i_children.append(newnode)
            names[(ch.i_module.i_modulename, ch.arg)] = True
    stmt.i_base_type_expanded = True

    refined = {}
    # then apply all refinements

    for refinement in stmt.search('refine'):
        # process 'refine' statements
        target = find_target_node(ctx, refinement)
        if target is None:
            continue
        if target in refined:
            err_add(ctx.errors, refinement.pos, 'MULTIPLE_REFINE',
                (target.arg, refined[target]))
            continue
        if not hasattr(target, 'i_inherited'):
            err_add(ctx.errors, refinement.pos, 'REFINE_NOT_INHERITED',
                (target.arg, target.pos))
            continue
        refined[target] = refinement.pos

        def replace_from_refinement(target, refinement, keyword, \
            valid_keywords, v_fun=None, is_additional = False):

            """allow `keyword` as a refinement in `valid_keywords`"""
            new = refinement.search_one(keyword)
            if new is not None and target.keyword in valid_keywords:
                old = target.search_one(keyword)
                if old is not None and not is_additional:
                    target.substmts.remove(old)
                if v_fun is not None:
                    v_fun(target, new)
                target.substmts.append(new)
            elif new is not None:
                err_add(ctx.errors, refinement.pos, 'BAD_REFINEMENT',
                        (target.keyword, target.i_module.i_modulename,
                         target.arg, keyword))
                return

        def v_default(target, default):
            type = target.search_one('type')
            if (type is not None and type.i_type_spec is not None):
                defval = type.i_type_spec.str_to_val(ctx.errors,
                                                     default.pos,
                                                     default.arg)
                target.i_default = defval
                target.i_default_str = default.arg
                if defval is not None:
                    type.i_type_spec.validate(ctx.errors, default.pos, defval,
                        ' for the default value')

        replace_from_refinement(target, refinement, 'description',
            ['container', 'leaf', 'leaf-list', 'list', 'choice', 'case',
            'anyxml', (ct_module_name, str_instance),
            (ct_module_name, str_instance_list)])
        replace_from_refinement(target, refinement, 'reference',
            ['container', 'leaf', 'leaf-list', 'list',  'choice', 'case',
            'anyxml', (ct_module_name, str_instance),
            (ct_module_name, str_instance_list)])
        replace_from_refinement(target, refinement, 'config',
            ['container', 'leaf', 'leaf-list', 'list',  'choice', 'anyxml',
            (ct_module_name, str_instance),
            (ct_module_name, str_instance_list)])
        replace_from_refinement(target, refinement, 'presence', ['container'])
        replace_from_refinement(target, refinement, 'must',
            ['container', 'leaf', 'leaf-list', 'list',
            (ct_module_name, str_instance),
            (ct_module_name, str_instance_list)], is_additional = True)
        replace_from_refinement(target, refinement, 'default',
            ['leaf', 'choice'], v_default)
        replace_from_refinement(target, refinement, 'mandatory',
            ['leaf', 'choice', (ct_module_name, str_instance)])
        replace_from_refinement(target, refinement, 'min-elements',
            ['leaf-list', 'list', (ct_module_name, str_instance_list)])
        replace_from_refinement(target, refinement, 'max-elements',
            ['leaf-list', 'list', (ct_module_name, str_instance_list)])
        # replace all vendor-specific statements
        for s in refinement.substmts:
            if util.is_prefixed(s.keyword):
                old = target.search_one(s.keyword)
                if old is not None:
                    target.substmts.remove(old)
                target.substmts.append(s)

def find_target_node(ctx, stmt, is_instance_allowed = False):
    """Find the target node for the 'refine' or 'augment' statements"""
    parent = stmt.parent
    if stmt.arg == '.':
        return parent
    # parse the path into a list of two-tuples of (prefix,identifier)
    pstr = '/' + stmt.arg
    path = [(m[1], m[2]) for m in syntax.re_schema_node_id_part.findall(pstr)]
    node = parent
    # go down the path
    for (prefix, identifier) in path:
        if not is_instance_allowed and node is not parent and is_instation(node):
            err_add(ctx.errors, stmt.pos, 'BAD_REF_AUG', (node.arg, node.pos))
            return None

        module = statements.prefix_to_module(parent.i_module, prefix,
                    stmt.pos, ctx.errors)
        if module is None:
            return None
        child = statements.search_child(node.i_children,
                    module.i_modulename, identifier)
        if child is None:
            err_add(ctx.errors, stmt.pos, 'NODE_NOT_FOUND',
                (module.i_modulename, identifier))
            return None
        node = child
    return node
                
def post_copy_fnc(stmt, is_inherited = False):
    """Used to finish the copying of a node while instantiating.
    Called for every node after Statatement.copy() method.
    """
    def post_copy(old, new):
        # inline the definition into our module
        if hasattr(old, 'i_children'):
            new.i_children = []
        new.i_uniques = []
        if (is_inherited):
            new.i_inherited = True
        # copy the nested Position object
        new.pos = copy.copy(new.pos)
        new.pos.uses_pos = stmt.pos
        # build the i_children list of pointers
        if hasattr(old, 'i_children') \
                and not new.keyword == (ct_module_name, str_instance) \
                and not new.keyword == (ct_module_name, str_instance_list):
            names = {}
            for x in old.i_children:
                # check if this i_child is a pointer to a substmt
                if (x.i_module.i_modulename, x.arg) in names:
                    continue
                if x in old.substmts:
                    # if so, create an equivalent pointer
                    idx = old.substmts.index(x)
                    new.i_children.append(new.substmts[idx])
                else:
                    # otherwise, copy the i_child
                    newx = x.copy(new, stmt,
                                  nocopy=['type','uses', 'unique',
                                          'typedef','grouping'],
                                  copyf=post_copy)
                    new.i_children.append(newx)
                names[(x.i_module.i_modulename, x.arg)] = True
    return post_copy

def v_instantiate_instance(ctx, stmt):
    """Instantiate 'instance' and 'instance-list' statements"""
    # check whether the node is nested into a complex type definition
    node = stmt
    while node is not None:
        if node.keyword == (ct_module_name, str_complex_type):
            break
        node = node.parent

    # label the complex type as used
    if node is not None:
        node.i_labeled = True
    # instantiate 'instance'/'instance-list' statement
    instantiate(ctx, stmt)
    # remove the label
    if node is not None:
        node.i_labeled = False
    return 'continue'

def instantiate(ctx, stmt):
    """Instantiate 'instance' and 'instance-list' statements"""
    if hasattr(stmt, 'is_grammatically_valid') \
            and not stmt.is_grammatically_valid:
        return
    ct = stmt.i_complex_type
    
    # if the reffered comple type has not been resolved or is not valid,
    # there is nothing to instantiate
    if not ct is None and  ct.is_grammatically_valid:
        # circular dependency, stop expanding here
        if hasattr(ct, 'i_labeled') and ct.i_labeled:
            stmt.i_recursion = True
            return

        ct.i_labeled = True
        # copy the complex type definitions into stmt.i_children
        names = {}
        for ch in ct.i_children:
            # don't copy the type since it cannot be modified anyway.
            if (ch.i_module.i_modulename, ch.arg) in names:
                continue
            newnode = ch.copy(stmt, stmt,
                          nocopy=['type','uses','unique','typedef','grouping'],
                          copyf=post_copy_fnc(stmt))
            stmt.i_children.append(newnode)
            names[(ch.i_module.i_modulename, ch.arg)] = True

        # if the reffered complex type has not been expanded,
        # expand the children of the 'stmt' statement
        dfs_instance(ctx, stmt)
        ct.i_labeled = False

    # apply aumentations to the instantiated node
    expand_augmentations(ctx, stmt)

def dfs_instance(ctx, stmt):
    """Recursively instantiate 'instance' and 'instance-list' statements"""
    if not hasattr(stmt, 'i_children'):
        return
    for ch in stmt.i_children:
        if is_instation(ch):
            instantiate(ctx, ch)
        else:
            dfs_instance(ctx, ch)

def expand_augmentations(ctx, stmt):
    """Apply augmentations to the instantiated node"""
    agms = stmt.search('augment')
    # parse the argument of augmentations
    stmts = []

    for a in agms:
        if a.arg.startswith("/"):
            err_add(ctx.errors, a.pos, 'BAD_VALUE',
                (a.arg, "descendant-node-id"))
            continue
        stmts += [a]
        arg = "/" + a.arg # to make node_id_part below work
        path = [(m[1], m[2]) \
                    for m in syntax.re_schema_node_id_part.findall(arg)]
        a.i_path = path

    # sort the list of augmentations to apply them in the properly order
    def cmp_len(a, b):
        if len(a.i_path) > len(b.i_path):
            return 1
        elif len(a.i_path) == len(b.i_path):
            return 0
        else:
            return -1
    stmts.sort(cmp_len)

    for a in stmts:
        v_reference_load_recursive_nodes(ctx, a)
        target = find_target_node(ctx, a, True)
        a.i_target_node = target

        if target is None:
            # inherit properies to avoid runtime errors
            # when a node does not have a conf value
            statements.v_inherit_properties(ctx, a)
            continue

        if not hasattr(target, 'i_children'):
            err_add(ctx.errors, a.pos, 'BAD_NODE_IN_AUGMENT',
                (target.i_module.arg, target.arg, target.keyword))
            continue

        # instantiate 'instance'/'instance-list' within the augmentation
        dfs_instance(ctx, a)

        # copy the augmentation definitions into target.i_children
        for c in a.i_children:
            if is_mandatory(c) and stmt.i_module.i_modulename != target.i_module.i_modulename:
                err_add(ctx.errors, c.pos, 'MANDATORY_AUGMENTATION',
                    (c.arg, c.pos))
                continue
            ch = statements.search_child(target.i_children,
                            stmt.i_module.i_modulename, c.arg)
            if ch is not None:
                err_add(ctx.errors, c.pos, 'DUPLICATE_CHILD_NAME',
                        (stmt.arg, stmt.pos, c.arg, ch.pos))
                continue
            elif target.keyword == 'choice' and c.keyword != 'case':
                # create an artifical case node for the shorthand
                new_case = statements.create_new_case(ctx, target, c)
                new_case.parent = target
            else:
                target.i_children.append(c)
                c.parent = target        

def v_inherit_properties_complex_type(ctx, ct):
    """ Inherit 'config' properties for statements nested into a
    'complex-type'. Set 'i_config' values. The logic is taken from
    statements.v_inherit_properties(), which can not be reused here because
    some constants are hardcoded in the implementation and here we use the
    proper values for them.
    """
    def iter(s, config_value, allow_explicit):
        cfg = s.search_one('config')
        if cfg is not None:
            if config_value is None and not allow_explicit:
                err_add(ctx.errors, cfg.pos, 'CONFIG_IGNORED', ())
            elif cfg.arg == 'true' and config_value == False:
                err_add(ctx.errors, cfg.pos, 'INVALID_CONFIG', ())
            elif cfg.arg == 'true':
                config_value = True
            elif cfg.arg == 'false':
                config_value = False
        s.i_config = config_value
        if (hasattr(s, 'is_grammatically_valid') and
            s.is_grammatically_valid == False):
            return
        if statements.is_keyword_with_children(s.keyword):
            for ch in s.search('grouping'):
                iter(ch, None, True)
            for ch in s.i_children:
                iter(ch, config_value, allow_explicit)

    for s in ct.search('grouping'):
        iter(s, None, True)
    for s in ct.i_children:
        iter(s, None, True)

def v_pre_expand_2_augment(ctx, stmt):
    """Validate whether the augment statement refers to
    an 'instance'/'instance-list'
    """
    # the statement has been already validated
    # the statement is nested into an 'instance'/'  instance-list'
    if is_instation(stmt.parent):
        stmt.i_target_node = None
        return

    # check whether the augment statement is applied to the inner part
    # of a instance/instance-list, if so, report an error
    ret = is_path_to_instance(ctx, stmt)

    if ret is not None:
        (ai, path) = ret
        stmt.i_target_node = None
        err_add(ctx.errors, stmt.pos, 'BAD_NODE_IN_AUGMENT',
            (ai.i_module.i_modulename, ai.arg, ai.keyword))

def v_unique_name_complex_type(ctx, stmt):
    """Make sure that all top-level definitions in a module (including all its
    submodules) are not duplicated in its submodules at the levels different
    from the top level (top-level definitions have been already validated)
    """
    defs = [((ct_module_name, str_complex_type),
            'COMPLEX_TYPE_ALREADY_DEFINED', stmt.i_complex_types)]
    def f(s):
        for (keyword, errcode, dict) in defs:
            if s.keyword == keyword and s.arg in dict:
                err_add(ctx.errors, dict[s.arg].pos,
                    errcode, (s.arg, s.pos))

    for i in stmt.search('include'):
        submodulename = i.arg
        subm = ctx.get_module(submodulename)
        if subm is not None:
            for s in subm.substmts:
                # don't validate the top-level definitions,
                # since it has been already done;
                for ss in s.substmts:
                    statements.iterate_stmt(ss, f)

def v_reference_complex_type(ctx, stmt):
    """Validate the key of the complex type"""
    key = stmt.search_one('key')
    if key is not None:
        # check that the key has not been defined in the base type
        basetype = stmt.i_base_type
        while basetype is not None:
            basekey = basetype.search_one('key')
            if basekey is not None:
                err_add(ctx.errors, key.pos, "REDEFINED_KEY",
                    (stmt.arg, basetype.arg, basekey.pos))
                return
            basetype =  basetype.i_base_type
        # check the references to the leafs
        # this part is based on the source code for the list's key validation
        stmt.i_key = []
        if key is not None:
            found = []
            for x in key.arg.split():
                if x == '':
                    continue
                if x.find(":") == -1:
                    name = x
                else:
                    [prefix, name] = x.split(':', 1)
                    if prefix != stmt.i_module.i_prefix:
                        err_add(ctx.errors, key.pos, 'BAD_KEY', x)
                        return

                ptr = attrsearch(name, 'arg', stmt.i_children)
                if x in found:
                    err_add(ctx.errors, key.pos, 'DUPLICATE_KEY', x)
                    return
                elif ((ptr is None) or (ptr.keyword != 'leaf')):
                    err_add(ctx.errors, key.pos, 'BAD_KEY', x)
                    return
                type = ptr.search_one('type')
                if type is not None:
                    t = statements.has_type(type, ['empty'])
                    if t is not None:
                        err_add(ctx.errors, key.pos, 'BAD_TYPE_IN_KEY',
                                (t.arg, x))
                        return
                default = ptr.search_one('default')
                if default is not None:
                    err_add(ctx.errors, default.pos, 'KEY_HAS_DEFAULT', ())

                stmt.i_key.append(ptr)
                found.append(x)

def v_reference_instance_list(ctx, stmt):
    """Verify that the complex-type referred to by an instance-list statement
    with config true must have a defined key
    """
    if stmt.i_config:
        ct = stmt.i_complex_type
        if ct is None:
            return
        while ct is not None:
            if hasattr(ct, 'i_key'):
                return
            ct = ct.i_base_type
        err_add(ctx.errors, stmt.pos, 'KEY_REQUIRED',
            (stmt.i_complex_type.arg, stmt.pos))

def v_reference_load_recursive_nodes(ctx, stmt):
    """Expands recursive data structures"""
    if stmt.arg.startswith("/"):
        is_absolute = True
        arg = stmt.arg
    else:
        is_absolute = False
        arg = "/" + stmt.arg
    path = [(m[1], m[2]) \
                for m in syntax.re_schema_node_id_part.findall(arg)]

    (prefix, identifier) = path[0]
    module = module_by_prefix(stmt.i_module, prefix)
    if module is None:
        return None

    # find the first node
    if (is_absolute):
        node = statements.search_child(module.i_children,
                module.i_modulename, identifier)
        if node is None:
            # check all our submodules
            for inc in module.search('include'):
                submod = ctx.get_module(inc.arg)
                if submod is not None:
                    node = statements.search_child(submod.i_children,
                                submod.i_modulename, identifier)
                    if node is not None:
                        break
            if node is None:
                return
    else:
        node = statements.search_child(stmt.parent.i_children, module.i_modulename, identifier)
        if node is None:
            return

    # then recurse down the path
    index = 1
    for (prefix, identifier) in path[1:]:
        index =+ 1

        # expand 'instance'/'instance-list' if required
        if hasattr(node, 'i_recursion'):
            instantiate(ctx, node)
            del node.i_recursion
            pass

        if hasattr(node, 'i_children'):
            module = module_by_prefix(stmt.i_module, prefix)
            if module is None:
                return None
            child = statements.search_child(node.i_children,
                        module.i_modulename, identifier)
            if child is None and module == stmt.i_module:
                # this node may be created - the solution has to be found
                return
            elif child is None:
                return
            node = child
        else:
            return
    return None


def v_unused_complex_type(ctx, stmt):
    """Adds warnings for complex types which are defined not
    at the top-level of the module and unused
    """
    if stmt.parent.parent is not None:
        # this is a locally scoped complex type
        if stmt.i_is_unused == True:
            err_add(ctx.errors, stmt.pos,
                    'UNUSED_COMPLEX_TYPE', stmt.arg)

complex_types_stmts = [
    # (<keyword>, <occurance when used>,
    #  (<argument type name | None>, <substmts>),
    #  <list of keywords where <keyword> can occur>)
    
    ('complex-type', '*',
     ('identifier', [
          ((ct_module_name, 'abstract'), '?'),
          ('anyxml', '*'), ('choice', '*'), ('container', '*'),
          ('description', '?'), ((ct_module_name, 'instance'), '*'),
          ((ct_module_name, 'instance-list'), '*'),
          ((ct_module_name, 'extends'), '?'), ('grouping', '*'),
          ('if-feature', '*'), ('key', '?'), ('leaf', '*'), ('leaf-list', '*'),
          ('list', '*'), ('ordered-by', '?'), ('reference', '?'),
          ('must', '*'), ('refine', '*'), ('status', '?'), ('typedef', '*'),
          ('uses', '*')
          # optional-feature - not yet supported by pyang
      ]),
     ['module', 'submodule', 'container', 'list', 'rpc', 'input', 'output',
     'notification']
     ),
    
    ('extends', '?',
     ('identifier-ref', [
           ('description', '?'), ('reference', '?'), ('status', '?')
     ]),
     []
     ),

    ('abstract', '?',
     ('boolean', []),
     []
     ),

    ('instance', '*',
     ('identifier', [
           ('description', '?'), ('config', '?'), ('if-feature', '*'), 
           ('mandatory', '?'), ('must', '*'), 
           ('reference', '?'), ('status', '?'), ('when', '?'),
           ('augment', '*'),
           ('container', '*'), ('leaf', '*'), ('leaf-list', '*'),
           ('list', '*'), ('uses', '*'), ('anyxml', '*'), ('choice', '*'),
           ((ct_module_name, str_instance_type), '1'),
           ((ct_module_name, str_instance), '*'), ((ct_module_name, str_instance_list), '*'),
     ]),
     [ 'module',  'submodule', 'grouping', 'container', 'list', 'case',
     'notification', 'input', 'output', 'augment', 'choice', 'case']
     ),

    ('instance-list', '*',
     ('identifier', [
           ('description', '?'), ('config', '?'), ('if-feature', '*'), 
           ('max-elements', '?'), ('min-elements', '?'), ('must', '*'),
           ('ordered-by', '?'), ('reference', '?'), ('status', '?'),
           ('when', '?'), ('augment', '*'), ('container', '*'),
           ('leaf', '*'), ('leaf-list', '*'), ('list', '*'), ('uses', '*'),
           ('anyxml', '*'), ('choice', '*'),
           ((ct_module_name, str_instance_type), '1'),
           ((ct_module_name, str_instance), '*'), ((ct_module_name, str_instance_list), '*'),
     ]),
     [ 'module',  'submodule', 'grouping', 'container', 'list', 'case',
     'notification', 'input', 'output', 'augment', 'choice']
     ),

    (str_instance_type, '?',
     ('identifier-ref', []),
     ['type']
     )
]  
    
# Utility funcions
def search_complex_type(stmt, name):
    """Searches for a complex-type in the scope of stmt."""
    while stmt is not None:
        if name in stmt.i_complex_types:
            return stmt.i_complex_types[name]
        stmt = stmt.parent
    return None

def resolve_complex_type(ctx, prefix, name, stmt):
    """Resolves a complex type by prefix and name in the scope of stmt."""
    if prefix is None or stmt.i_module.i_prefix == prefix:
        # check local complex types
        stmt.i_complex_type = search_complex_type(stmt, name)
    else:
        # this is a prefixed name, check the imported modules
        pmodule = statements.prefix_to_module(stmt.i_module, prefix, stmt.pos,
                    ctx.errors)
        if pmodule is None:
            return
        stmt.i_complex_type = search_complex_type(pmodule, name)

def validate_complex_type(ctx, stmt):
    """Validates a reffered complex type."""
    if stmt.i_complex_type is None:
        err_add(ctx.errors, stmt.pos,
                'COMPLEX_TYPE_NOT_FOUND', (stmt.arg, stmt.i_module.arg))
        return
    else:
        # ensure the complex type is validated
        if stmt.i_complex_type.is_grammatically_valid == True:
            v_type_complex_type(ctx, stmt.i_complex_type)
        stmt.i_complex_type.i_is_unused = False


def get_identifier_ref(ref):
    """Parses an indentifier reference given in the form '[prefix:]name'"""
    if ref.find(":") == -1:
        prefix = None
        name = ref
    else:
        [prefix, name] = ref.split(':', 1)
    return (prefix, name)

def is_path_to_instance(ctx, stmt):
    """Validate whether the argument of the augmentation does not
    point into the inner part of an 'instance'/'instance-list'
    """
    if stmt.arg.startswith("/"):
        is_absolute = True
        arg = stmt.arg
    else:
        is_absolute = False
        arg = "/" + stmt.arg # to make node_id_part below work

    # parse the path into a list of two-tuples of (prefix,identifier)
    path = [(m[1], m[2]) for m in syntax.re_schema_node_id_part.findall(arg)]
    # find the module of the first node in the path
    (prefix, identifier) = path[0]
    module = module_by_prefix(stmt.i_module, prefix)
    if module is None:
        return None

    if (stmt.parent.keyword in ('module', 'submodule') or is_absolute):
        # find the first node
        node = statements.search_child(module.i_children, module.i_modulename,
                    identifier)
        if node is None:
            # check all our submodules
            for inc in module.search('include'):
                submod = ctx.get_module(inc.arg)
                if submod is not None:
                    node = statements.search_child(submod.i_children,
                                submod.i_modulename, identifier)
                    if node is not None:
                        break
            if node is None:
                return None
    else:
        chs = [c for c in stmt.parent.parent.i_children \
                   if hasattr(c, 'i_uses') and c.i_uses == stmt.parent]
        node = statements.search_child(chs, module.i_modulename, identifier)
        if node is None:
            return None

    if (is_instation(node)):
        return (node, path[1:])

    # then recurse down the path
    index = 1
    for (prefix, identifier) in path[1:]:
        index =+ 1
        if hasattr(node, 'i_children'):
            module = module_by_prefix(stmt.i_module, prefix)
            if module is None:
                return None
            child = statements.search_child(node.i_children,
                        module.i_modulename, identifier)
            if child is None and module == stmt.i_module:
                # this node may be created - the solution has to be found
                return None
            elif child is None:
                return None
            node = child
            if (is_instation(node)):
                return (node, path[index:])
        else:
            return None
    return None

def module_by_prefix(module, prefix):
    """Resolve a module by the prefix without error notifications"""
    if prefix == '':
        return module
    try:
        (modulename, revision) = module.i_prefixes[prefix]
    except KeyError:
        return None
    return module.i_ctx.get_module(modulename, revision)


def is_complex_type(stmt):
    """Check whether the statement is a 'complex-type' definition"""
    return stmt.keyword == (ct_module_name, str_complex_type)

def is_instance(stmt):
    """Check whether the statement is an 'instance' definition"""
    return stmt.keyword == (ct_module_name, str_instance)

def is_instance_list(stmt):
    """Check whether the statement is an 'instance-list' definition"""
    return stmt.keyword == (ct_module_name, str_instance_list)

def is_instation(stmt):
    """Check whether the statement is an 'instance' or
    'instance-list' definition
    """
    return is_instance(stmt) or is_instance_list(stmt)

def is_mandatory(stmt):    
    """Check whether the statement is a mandatory node"""
    if stmt.keyword in ["leaf", "choice", "anyxml", \
            (ct_module_name, str_instance)] and \
            stmt.search_one("mandatory", 'true') is not None:
        return True
    elif stmt.keyword in ["leaf-list", "list", \
            (ct_module_name, str_instance_list)]:
        me = stmt.search_one("min-elements")
        if me is not None and int(me.arg) > 0:
            return True
    elif stmt.keyword in ["container"] and \
            stmt.search_one("presence") is not None:
        for ch in stmt.i_children:
            if is_mandatory(ch):
                return True       
    return False