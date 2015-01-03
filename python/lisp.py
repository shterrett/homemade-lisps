Symbol = str
import math, operator as op
import code

List = list
Number = (int, float)

def tokenize(chars):
  "Convert string into list of tokens"
  return chars.replace('(', ' ( ').replace(')', ' ) ').split()

def parse(program):
  "Read Scheme program from string"
  return read_from_tokens(tokenize(program))

def read_from_tokens(tokens):
  "Read expr from series of tokens"
  if len(tokens) == 0:
    raise SyntaxError('Unexpected EOF')
  token = tokens.pop(0)
  if token == '(':
    L = []
    while tokens[0] != ')':
      L.append(read_from_tokens(tokens))
    tokens.pop(0)
    return L
  elif token == ')':
    raise SyntaxError('Unexpected ")"')
  else:
    return atom(token)

def atom(token):
  "Numbers become numbers; everything else is a symbol"
  try: return int(token)
  except ValueError:
    try: return float(token)
    except ValueError:
      return Symbol(token)

class Procedure(object):
  def __init__(self, parms, body, env):
    self.parms, self.body, self.env = parms, body, env
  def __call__(self, *args):
    return eval(self.body, Env(self.parms, args, self.env))

class Env(dict):
  def __init__(self, parms=(), args=(), outer=None):
    self.update(zip(parms, args))
    self.outer = outer
  def find(self, var):
    return self if (var in self) else self.outer.find(var)

def standard_env():
  "An environment with some Scheme standard procedures."
  env = Env()
  env.update(vars(math)) # sin, cos, sqrt, pi, ...
  env.update({
              '+':       op.add,
              '-':       op.sub,
              '*':       op.mul,
              '/':       op.truediv,
              '>':       op.gt,
              '<':       op.lt,
              '>=':      op.ge,
              '<=':      op.le,
              '=':       op.eq,
              'abs':     abs,
              'append':  op.add,
              'apply':   apply,
              'begin':   lambda *x: x[-1],
              'car':     lambda x: x[0],
              'cdr':     lambda x: x[1:],
              'cons':    lambda x,y: [x] + y,
              'eq?':     op.is_,
              'equal?':  op.eq,
              'length':  len,
              'list':    lambda *x: list(x),
              'list?':   lambda x: isinstance(x,list),
              'map':     map,
              'max':     max,
              'min':     min,
              'not':     op.not_,
              'null?':   lambda x: x == [],
              'number?': lambda x: isinstance(x, Number),
              'procedure?': callable,
              'round':   round,
              'symbol?': lambda x: isinstance(x, Symbol),
             })
  return env

def apply(func, args, keywords):
  return func(*args, **keywords)

global_env = standard_env()

def eval(x, env=global_env):
  "Evaluate an expression in an environment"
  if isinstance(x, Symbol):
    return env.find(x)[x]
  elif not isinstance(x, List):
    return x
  elif x[0] == 'quote':
    (_, exp) = x
    return exp
  elif x[0] == 'if':
    (_, test, conseq, alt) = x
    exp = conseq if eval(test, env) else alt
    return eval(exp, env)
  elif x[0] == 'define':
    (_, var, exp) = x
    val = eval(exp, env)
    env[var] = val
    return None
  elif x[0] == 'set!':
    (_, var, exp) = x
    evaled = eval(exp, env)
    env.find(var)[var] = evaled
    return evaled
  elif x[0] == 'lambda':
    (_, params, body) = x
    return Procedure(params, body, env)
  else:
    proc = eval(x[0], env)
    args = [eval(arg, env) for arg in x[1:]]
    return proc(*args)

def repl(prompt="> "):
  while True:
    input = code.InteractiveConsole.raw_input(prompt)
    val = eval(parse(input))
    if val is not None:
      print(to_string(val))

def to_string(exp):
  if isinstance(exp, List):
    return '(' + " ".join(map(to_string, exp)) + ')'
  else:
    return str(exp)
