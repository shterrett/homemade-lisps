Symbol = str
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
    token.pop(0)
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
