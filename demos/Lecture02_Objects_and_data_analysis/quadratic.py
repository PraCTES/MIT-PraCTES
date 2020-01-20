
def quadratic_roots(a, b, c):
    x1 = (-b + (b**2 - 4. *a*c)**0.5)/(2*a)
    x2 = (-b - (b**2 - 4. *a*c)**0.5)/(2*a)
    return x1, x2 # we can return more than one variable

class quadratic():
    def __init__(self, a, b, c):
        self.a = a
        self.b = b
        self.c = c
        
    def evaluate(self, x):
        return self.a * x**2 + self.b * x + self.c
    
    def calc_roots(self):
        return quadratic_roots(self.a, self.b, self.c)

class parabola(quadratic):
    def __init__(self, a, c):
        self.a = a
        self.b = 0.
        self.c = c