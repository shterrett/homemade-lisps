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

def let(*args):
  args = list(args)
  x = cons(_let, args)
  require(x, len(args)>1)
  bindings, body = args[0], args[1:]
  require(x, all(isa(b, list) and len(b) == 2 and isa(b[0], Symbol)
    for b in bindings), "illegal binding list")
  vars, vals = zip(*bindings)
  return [[_lambda, list(vars)] + map(expand, body)] + map(expand, vals)

_append, _cons, _let = map(Sym, ["append", "cons", "let"])

macro_table = {_let:let}

eval(parse("""(begin
  (define-macro and (lambda args
    (if (null? args)
      (if (= (length args) 1) (car args)
        `(if ,(car args) (and ,@(cdr args)) #f))))))"""))

def eval(x, env=global_env):
  while True:
    if isa(x, Symbol):
      return env.find(x)[x]
    elif not isa(x, list):
      return x
    elif x[0] is _quote:
      (_, exp) = x
      return exp
    elif x[0] is _if:
      (_, test, conseq, alt) = x
      x = (conseq if eval(test, env) else alt)
    elif x[0] is _set:
      (_, var, exp) = x
      val = eval(exp, en)
      env.find(var)[var] = val
      return val
    elif x[0] is _define:
      (_, var, exp) = x
      val = eval(exp, env)
      env[var] = val
      return val
    elif x[0] is _lambda:
      (_, vars, exp) = x
      return Procedure(vars, exp, env)
    elif x[0] is _begin:
      for exp in x[1:-1]:
        eval(exp, env)
      x = x[-1]
    else:
      exps = [eval(exp, env) for exp in x]
      proc = exps.pop(0)
      if isa(proc, Procedure):
        x = proc.exp
        env = Env(proc.params, exps, proc.env)
      else:
        return proc(*exps)

class Procedure(object):
  def __init__(self, params, exp, env):
    self.params = params
    self.exp = exp
    self.env = env

  def __call__(self, *args):
    return eval(self.exp, Env(self.params, args, self.env))

def callcc(proc):
  ball = RuntimeWarning("Sorry, can't continue this continuation any longer")
  def throw(retval):
    ball.retval = retval; raise ball
  try:
    return(proc(throw))
  except RuntimeWarning as w:
    if w is ball:
      return ball.retval
    else:
      raise w

class Env(dict):
  def __init__(self, parms=(), args=(), outer=None):
    self.outer = outer
    if isa(parms, Symbol):
      self.update({parms:list(args)})
    else:
      if len(args) != len(parms):
        raise TypeError('expected {0}, given {1}'.format(to_string(parms), to_string(args)))
      self.update(zip(parms, args))

  def find(self, var):
    if var in self:
      return self
    elif self.outer is None:
      raise LookupError(var)
    else:
      return self.outer.find(var)

def parse(inport):
  if isinstance(inport, str):
    inport = InPort(StringIO.StringIO(inport))
  return expand(read(inport), topLevel=True)


def expand(x, toplevel=False):
    require(x, x!=[])
    if not isa(x, list):
        return x
    elif x[0] is _quote:
        require(x, len(x)==2)
        return x
    elif x[0] is _if:
        if len(x)==3: x = x + [None]
        require(x, len(x)==4)
        return map(expand, x)
    elif x[0] is _set:
        require(x, len(x)==3);
        var = x[1]
        require(x, isa(var, Symbol), "can set! only a symbol")
        return [_set, var, expand(x[2])]
    elif x[0] is _define or x[0] is _definemacro:
        require(x, len(x)>=3)
        _def, v, body = x[0], x[1], x[2:]
        if isa(v, list) and v:
            f, args = v[0], v[1:]
            return expand([_def, f, [_lambda, args]+body])
        else:
            require(x, len(x)==3)
            require(x, isa(v, Symbol), "can define only a symbol")
            exp = expand(x[2])
            if _def is _definemacro:
                require(x, toplevel, "define-macro only allowed at top level")
                proc = eval(exp)
                require(x, callable(proc), "macro must be a procedure")
                macro_table[v] = proc
                return None
            return [_define, v, exp]
    elif x[0] is _begin:
        if len(x)==1: return None
        else: return [expand(xi, toplevel) for xi in x]
    elif x[0] is _lambda:
        require(x, len(x)>=3)
        vars, body = x[1], x[2:]
        require(x, (isa(vars, list) and all(isa(v, Symbol) for v in vars))
                or isa(vars, Symbol), "illegal lambda argument list")
        exp = body[0] if len(body) == 1 else [_begin] + body
        return [_lambda, vars, expand(exp)]
    elif x[0] is _quasiquote:
        require(x, len(x)==2)
        return expand_quasiquote(x[1])
    elif isa(x[0], Symbol) and x[0] in macro_table:
        return expand(macro_table[x[0]](*x[1:]), toplevel)
    else:
        return map(expand, x)

def require(x, predicate, msg="wrong length"):
  if not predicate:
    raise SyntaxError(to_string(x) + ': ' + msg)

def expand_quasiquote(x):
  """Expand `x => 'x; `,x => x; `(,@x y) => (append x y) """
  if not is_pair(x):
    return [_quote, x]
  require(x, x[0] is not _quoteunsplicing, "Cannot unsplice here")
  if x[0] is unquote:
    require(x, len(x) == 2)
    return x[1]
  elif is_pair(x) and x[0] is _unquotesplicing:
    require(x[0], len(x[0]) == 2)
    return [_append, x[0][1], expand_quasiquote(x[1:])]

def add_globals(self):
  "Add some Scheme standard procedures."
  import math, cmath, operator as op
  self.update(vars(math))
  self.update(vars(cmath))
  self.update({
    '+':op.add, '-':op.sub, '*':op.mul, '/':op.div, 'not':op.not_,
    '>':op.gt, '<':op.lt, '>=':op.ge, '<=':op.le, '=':op.eq,
    'equal?':op.eq, 'eq?':op.is_, 'length':len, 'cons':lambda x,y:[x]+list(y),
    'car':lambda x:x[0], 'cdr':lambda x:x[1:], 'append':op.add,
    'list':lambda *x:list(x), 'list?': lambda x:isa(x,list),
    'null?':lambda x:x==[], 'symbol?':lambda x: isa(x, Symbol),
    'boolean?':lambda x: isa(x, bool), 'pair?':is_pair,
    'port?': lambda x:isa(x,file), 'apply':lambda proc,l: proc(*l),
    'eval':lambda x: eval(expand(x)), 'load':lambda fn: load(fn), 'call/cc':callcc,
    'open-input-file':open,'close-input-port':lambda p: p.file.close(),
    'open-output-file':lambda f:open(f,'w'), 'close-output-port':lambda p: p.close(),
    'eof-object?':lambda x:x is eof_object, 'read-char':readchar,
    'read':read, 'write':lambda x,port=sys.stdout:port.write(to_string(x)),
    'display':lambda x,port=sys.stdout:port.write(x if isa(x,str) else to_string(x))})
  return self

global_env = add_globals(Env())

def is_pair(x):
  return x != [] and isa(x, list)

isa = isinstance
