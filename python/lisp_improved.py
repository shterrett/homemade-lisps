class Symbol(str): pass

def Sym(s, symbol_table={}):
  "Find or create entry in symbol_table for s)"
  if s not in symbol_table: symbol_table[s] = Symbol(s)
  return symbol_table[s]

_quote, _if, _set, _define, _lambda, _begin, _definemacro, = map(Sym,
    "quote   if   set!  define   lambda   begin   define-macro".split())

_quasiquote, _unquote, _unquotesplicing = map(Sym,
    "quasiquote   unquote   unquote-splicing".split())

class InPort(object):
  "An input port; retains a line of characters"
  tokenizer = r'''\s*(,@|[('`,)]|"(?:[\\].|[^\\"])*"|;.*|[^\s('"`,;)]*)(.*)'''

  def __init__(self, file):
    self.file = file
    self.line = ''

  def next_token(self):
    "Return next token, reading new text into line buffer if needed"
    while True:
      if self.line == '': self.file.readline()
      if self.line == '': return eof_object
      token, self.line = re.match(InPort.tokenizer, self.line).groups()
      if token != '' and not token.startswith(';'):
        return token

eof_object = Symbol('#<eof-object>')

def readchar(inport):
  if inport.line != '':
    ch, inport.line = inport.line[0], inport.line[1:]
    return ch
  else:
    return inport.file.read(1) or eof_object

def read(inport):
  def read_ahead(token):
    if token == '(':
      L = []
      # consider:
      # while token != ')':
      #   token = inport.next_token()
      #   L.append(token)
      # return L
      while True:
        token = inport.next_token()
        if token == ')': return L
        else: L.append(read_ahead(token))
    elif token == ')': raise SyntaxError('unexpected )')
    elif token in quotes: return [quotes[token], read(inport)]
    elif token == eof_object: raise SyntaxError('unexpected EOF')
    else: return atom(token)
  token1 = inport.next_token()
  return eof_object if token1 == eof_object else read_ahead(token1)

quotes = {"'":_quote, "`":_quasiquote, ",":_unquote, ",@":_unquotesplicing}
