import math

class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def size(self):
        return math.sqrt(self.x ** 2 + self.y ** 2)

    def same_radius(self, other):
        return self.size() == other.size()
