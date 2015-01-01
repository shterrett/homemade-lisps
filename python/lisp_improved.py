import sys

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

def atom(token):
  if token == "#t": return True
  elif token == "#f": return False
  elif token == '"': return token[1:-1].decode('string_escape')
  try: return int(token)
  except ValueError:
    try: return float(token)
    except ValueError:
      return Sym(token)

def to_string(x):
  if x is True: return "#t"
  elif x is False: return "#f"
  elif isa(x, Symbol): return x
  elif isa(x, str): return '"%s"' % x.encode('string_escape').replace('"', r'\"')
  elif isa(x, list): return '(' + ' '.join(map(to_string, x)) + ')'
  elif isa(x, complex): return str(x).replace('j', 'i')
  else: return str(x)

def load(filename):
  repl(None, InPort(open(filename)), None)

def repl(prompt='> ', inport=InPort(sys.stdin), out=sys.stdout):
  out.write("Lispy version 2.0\n")
  while True:
    try:
      if prompt:
        out.write(prompt)
      x = parse(inport)
      if x is eof_object: return
      val = eval(x)
      if val is not None and out: print >> out, to_string(val)
    except Exception as e:
      print("{0}: {1}".format(type(e).__name__, e))
