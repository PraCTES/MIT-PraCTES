class quadratic():
    def __init__(self, a, b, c):
        self.a = a
        self.b = b
        self.c = c
        
    def evaluate(self, x):
        return self.a * x**2 + self.b * x + self.c

class parabola(quadratic):
    def __init__(self, a, c):
        self.a = a
        self.b = 0.
        self.c = c