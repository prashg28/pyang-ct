# This pyang plugin generates a random XML instance document adhering
# to a YANG module.

from pyang import plugin
from pyang import types
from pyang import statements
import sys
from random import randint, random

def pyang_plugin_init():
    plugin.register_plugin(YANGXMLPlugin())

class YANGXMLPlugin(plugin.PyangPlugin):
    def add_output_format(self, fmts):
        fmts['xml'] = self
    def emit(self, ctx, module, writef):
        emit_xml(module, writef)
    
def emit_xml(module, fd):
    # pick one top-level child
    if len(module.i_children) == 0:
        return
    c = pick(module.i_children)
    attrs = ' xmlns="%s"' % module.search_one('namespace').arg
    cn = c.__class__.__name__
    if cn == 'Choice':
        print >> sys.stderr, "Cannot handle choice on top-level"
        sys.exit(1)
    emit_stmt(c, fd, '', attrs, 1)

def emit_stmt(stmt, fd, ind, attrs='', n=None):
    if n == None and stmt.keyword in ['list', 'leaf-list']:
        lb = 0
        if stmt.search_one('min_elements') != None:
            lb = int(stmt.search_one('min_elements').arg)
        ub = 3 # 3 is many
        if (stmt.search_one('max_elements') != None and
            stmt.search_one('max_elements').arg != 'unbounded'):
            ub = int(stmt.search_one('max_elements').arg)
            if ub > 3:
                ub = 3
        if lb > ub:
            ub = lb
        n = randint(lb,ub)
    if stmt.keyword == 'container':
        emit_container(stmt, fd, ind, attrs)
    elif stmt.keyword == 'leaf':
        emit_leaf(stmt, fd, ind, attrs)
    elif stmt.keyword == 'leaf-list':
        emit_leaf_list(stmt, n, fd, ind, attrs)
    elif stmt.keyword == 'list':
        emit_list(stmt, n, fd, ind, attrs)
    elif stmt.keyword == 'choice':
        # pick one case
        c = pick(stmt.i_children)
        emit_stmt(c, fd, ind)
    elif stmt.keyword == 'case':
        for c in stmt.i_children:
            emit_stmt(c, fd, ind)
    elif stmt.keyword == 'anyxml':
        emit_anyxml(stmt, fd, ind)
        
def emit_leaf(stmt, fd, ind, attrs):
    do_print = True
    if ((stmt.search_one('mandatory') != None) and 
        (stmt.search_one('mandatory').arg == 'false') or
        (stmt.search_one('default') != None) or 
        (statements.has_type(stmt.search_one('type'), ['empty']) != None)):
        if random() < 0.3:
            do_print = False
    if do_print == True:
        fd.write(ind + '<%s%s>' % (stmt.arg, attrs))
        emit_type_val(stmt.search_one('type'), fd)
        fd.write('</%s>\n' % stmt.arg)

def emit_container(stmt, fd, ind, attrs):
    do_print = True
    if stmt.search_one('presence') != None:
        if random() < 0.3:
            do_print = False
    if do_print == True:
        fd.write(ind + '<%s%s>\n' % (stmt.arg, attrs))
        for c in stmt.i_children:
            emit_stmt(c, fd, ind + '  ')
        fd.write(ind + '</%s>\n' % stmt.arg)

def emit_leaf_list(stmt, n, fd, ind, attrs):
    while (n > 0):
        fd.write(ind + '<%s%s>' % (stmt.arg, attrs))
        emit_type_val(stmt.search_one('type'), fd)
        fd.write('</%s>\n' % stmt.arg)
        n = n - 1

def emit_list(stmt, n, fd, ind, attrs):
    while (n > 0):
        fd.write(ind + '<%s%s>\n' % (stmt.arg, attrs))
        cs = stmt.i_children
        for k in stmt.i_key:
            fd.write(ind + '  <%s>' % k.arg)
            emit_type_val(k.search_one('type'), fd)
            fd.write('</%s>\n' % k.arg)
        for c in stmt.i_children:
            if c not in stmt.i_key:
                emit_stmt(c, fd, ind + '  ')
        fd.write(ind + '</%s>\n' % stmt.arg)
        n = n - 1

def emit_anyxml(stmt, fd, ind):
    fd.write(ind + '<%s><bar xmlns="">42</bar></%s>\n' % (stmt.arg, stmt.arg))
        
def emit_type_val(t, fd):
    if t.i_is_derived == False and t.i_typedef != None:
        return emit_type_val(t.i_typedef.search_one('type'), fd)
    inttype = statements.has_type(t, ['int8','int16','int32','int64',
                                      'uint8','uint16','uint32','uint64'])
    enum = t.search('enum')
    range = t.search_one('range')
    if enum != []:
        enum = pick(enum)
        fd.write(enum.arg)
    elif statements.has_type(t, ['empty']) != None:
        pass
    elif range != None:
        (lo,hi) = pick(t.i_ranges)
        ts = types.yang_type_specs[inttype.arg]
        if lo == 'min':
            lo = ts.min
        if hi == 'max':
            hi = ts.max
        if hi == None:
            hi = lo
        val = randint(lo,hi)
        fd.write(str(val))
    elif statements.has_type(t, ['boolean']) != None:
        val = pick(['true','false'])
        fd.write(val)
    elif inttype != None:
        ts = types.yang_type_specs[inttype.arg]
        val = randint(ts.min, ts.max)
        fd.write(str(val))
    elif statements.has_type(t, ['ipv4-address']) != None:
        fd.write('10.0.0.1')
    else:
        length = t.search_one('length')
        pattern = t.search('pattern')
        if length != None:
            # pick a length
            (lo,hi) = pick(t.i_lengths)
            if hi == None:
                hi = lo
            lentgh = randint(lo,hi)
        if pattern != []:
            # oh boy
            pass
        fd.write('__foo__')

def pick(xs):
    r = randint(0,len(xs)-1)
    return xs[r]
