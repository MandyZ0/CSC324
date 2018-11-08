def bisect(f, tol, a, b):
  # Precondition: f(a) and f(b) have different signs.
  c = (a + b)/2
  while abs(f(c)) >= tol:
    if sign(f(a)) == sign(f(c)):
      a = c
    else:
      b = c
    c = (a + b)/2

  return c


def sign(n):
  if n > 0:
    return 1
  elif n == 0:
    return 0
  else:
    return -1
