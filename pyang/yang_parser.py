""" This module implements a generic YANG parser.

The parser does not check any keywords or grammar.
"""
import error
import util
import statements
import syntax

class YangTokenizer(object):
    def __init__(self, text, pos, errors):
        self.lines = text.splitlines(True)
        self.pos = pos
        self.buf = ''
        self.offset = 0
        """Position on line.  Used to remove leading whitespace from strings."""
        
        self.errors = errors

    def readline(self):
        if len(self.lines) == 0:
            raise error.Eof
        self.buf = self.lines[0]
        del self.lines[0]
        self.pos.line += 1
        self.offset = 0

    def set_buf(self, i):
        self.offset = self.offset + i
        self.buf = self.buf[i:]

    def skip(self):
        """Skip whitespace and count position"""
        i = 0
        pos = 0
        buflen = len(self.buf)

        while True:
            self.buf = self.buf.lstrip()
            self.offset += (buflen - len(self.buf))
            if self.buf == '':
                self.readline()
                buflen = len(self.buf)
            else:
                break
            
        # skip line comment
        if self.buf[0] == '/':
            if self.buf[1] == '/':
                self.readline()
                return self.skip()
        # skip block comment
            elif self.buf[1] == '*':
                i = self.buf.find('*/')
                while i == -1:
                    self.readline()
                    i = self.buf.find('*/')
                self.set_buf(i+2)
                return self.skip()

    def get_keyword(self):
        """ret: identifier | (prefix, identifier)"""
        self.skip()

        m = syntax.re_keyword.match(self.buf)
        if m == None:
            error.err_add(self.errors, self.pos,
                          'SYNTAX_ERROR', 'illegal keyword: ' + self.buf)
            raise error.Abort
        else:
            self.set_buf(m.end())
            # check the separator
            if (self.buf[0].isspace() or
                (self.buf[0] == '/' and self.buf[1] in ('/', '*')) or
                (self.buf[0] in (';','{'))):
                pass
            else:
                error.err_add(self.errors, self.pos,
                              'SYNTAX_ERROR', 'expected separator, got: "' +
                              self.buf[:6] + '..."')
                raise error.Abort
            

            if m.group(2) == None: # no prefix
                return m.group(3)
            else:
                return (m.group(2), m.group(3))

    def peek(self):
        """Return next real character in input stream.

        Skips whitespace and comments, and returns next character
        without consuming it.  Use skip_tok() to consume the characater.
        """
        self.skip()
        try:
            return self.buf[0]
        except:
            raise error.Eof

    def skip_tok(self):
        self.skip()
        self.set_buf(1)
    
    def get_string(self, need_quote=False):
        """ret: string"""
        self.skip()
        
        if self.buf[0] == ';' or self.buf[0] == '{' or self.buf[0] == '}':
            error.err_add(self.errors, self.pos,
                          'EXPECTED_ARGUMENT', self.buf[0])
            raise error.Abort
        if self.buf[0] == '"' or self.buf[0] == "'":
            # for double-quoted string,  loop over string and translate
            # escaped characters.  also strip leading whitespace as
            # necessary.
            # for single-quoted string, keep going until end quote is found.
            quote_char = self.buf[0]
            # collect output in strs (list of strings)
            strs = [] 
            # remember position of " character
            indentpos = self.offset
            i = 1
            while True:
                buflen = len(self.buf)
                start = i
                while i < buflen:
                    if self.buf[i] == quote_char:
                        # end-of-string; copy the buf to output
                        strs.append(self.buf[start:i])
                        # and trim buf
                        self.set_buf(i+1)
                        # check for '+' operator
                        self.skip()
                        if self.buf[0] == '+':
                            self.set_buf(1)
                            self.skip()
                            nstr = self.get_string(need_quote=True)
                            if (type(nstr) != type('')):
                                error.err_add(self.errors, self.pos,
                                              'EXPECTED_QUOTED_STRING', ())
                                raise error.Abort
                            strs.append(nstr)
                        return ''.join(strs)
                    elif (quote_char == '"' and
                          self.buf[i] == '\\' and i < (buflen-1)):
                        # check for special characters
                        special = None
                        if self.buf[i+1] == 'n':
                            special = '\n'
                        elif self.buf[i+1] == 't':
                            special = '\t'
                        elif self.buf[i+1] == '\"':
                            special = '\"'
                        elif self.buf[i+1] == '\\':
                            special = '\\'
                        if special != None:
                            strs.append(self.buf[start:i])
                            strs.append(special)
                            i = i + 1
                            start = i + 1
                    i = i + 1
                # end-of-line, keep going
                strs.append(self.buf[start:i])
                self.readline()
                i = 0
                if quote_char == '"':
                    # skip whitespace used for indentation
                    buflen = len(self.buf)
                    while (i < buflen and self.buf[i].isspace() and
                           i <= indentpos):
                        i = i + 1
                    if i == buflen:
                        # whitespace only on this line; keep it as is
                        i = 0
        elif need_quote == True:
            error.err_add(self.errors, self.pos, 'EXPECTED_QUOTED_STRING', ())
            raise error.Abort
        else:
            # unquoted string
            buflen = len(self.buf)
            i = 0
            while i < buflen:
                if (self.buf[i].isspace() or self.buf[i] == ';' or
                    self.buf[i] == '{' or self.buf[i] == '}' or
                    self.buf[i:i+2] == '//' or self.buf[i:i+2] == '/*' or
                    self.buf[i:i+2] == '*/'):
                    res = self.buf[:i]
                    self.set_buf(i)
                    return res
                i = i + 1

class YangParser(object):
    def __init__(self, extra={}):
        pass

    def parse(self, ctx, ref, text):
        """Parse the string `text` containing a YANG statement.

        Return a Statement on success or None on failure
        """

        self.ctx = ctx
        self.pos = error.Position(ref)
        self.top = None

        try:
            self.tokenizer = YangTokenizer(text, self.pos, ctx.errors)
            stmt = self._parse_statement(None)
        except error.Abort:
            return None
        except error.Eof, e:
            error.err_add(self.ctx.errors, self.pos, 'EOF_ERROR', ())
            return None
        try:
            # we expect a error.Eof at this point, everything else is an error
            self.tokenizer.peek()
        except error.Eof:
            return stmt
        except:
            pass
        error.err_add(self.ctx.errors, self.pos, 'TRAILING_GARBAGE', ())
        return None

    def _parse_statement(self, parent):
        keywd = self.tokenizer.get_keyword()
        # check for argument
        tok = self.tokenizer.peek()
        if tok == '{' or tok == ';':
            arg = None
        else:
            arg = self.tokenizer.get_string()

        stmt = statements.Statement(self.top, parent, self.pos, keywd, arg)
        if self.top is None:
            self.pos.top_name = arg
            self.top = stmt
 
        # check for substatements
        tok = self.tokenizer.peek()
        if tok == '{':
            self.tokenizer.skip_tok() # skip the '{'
            while self.tokenizer.peek() != '}':
                substmt = self._parse_statement(stmt)
                stmt.substmts.append(substmt)
            self.tokenizer.skip_tok() # skip the '}'
        elif tok == ';':
            self.tokenizer.skip_tok() # skip the ';'
        else:
            error.err_add(self.ctx.errors, self.pos, 'INCOMPLETE_STATEMENT',
                          (keywd, tok))
            raise error.Abort
        return stmt

# FIXME: tmp debug
import sys

def ppkeywd(tok):
    if util.is_prefixed(tok):
        return tok[0] + ':' + tok[1]
    else:
        return tok

def pp(s, indent=0):
    sys.stdout.write(" " * indent + ppkeywd(s.raw_keyword))
    if s.arg is not None:
        sys.stdout.write(" '" + s.arg + "'")
    if s.substmts == []:
        sys.stdout.write(";\n")
    else:
        sys.stdout.write(" {\n")
        for ss in s.substmts:
            pp(ss, indent+4)
        sys.stdout.write(" " * indent + "}\n")
        
