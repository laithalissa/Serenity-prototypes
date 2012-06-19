__author__ = 'joseph'

class World:

    def __init__(self, size):
        radius = size / 2

        self.livingCells = set()

        # boudary of living cells
        self.boundingBox = (-radius, -radius, radius, radius)


    def width(self):
        xmin, _, xmax, _ = self.boundingBox
        return xmax - xmin

    def height(self):
        _, ymin, _, ymax = self.boundingBox
        return ymax - ymin


    def setCell(self,x,y,*args):

        living = not ((x,y) in self.livingCells) if len(args)==0 else args[0]
        if (x,y) in self.livingCells: self.livingCells.remove((x,y))
        if living:
            self.livingCells.add((x,y))

        xmin, ymin, xmax, ymax = self.boundingBox
        if x < xmin: xmin = x
        if x > xmax: xmax = x
        if y < ymin: ymin = y
        if y > ymax: ymax = y
        self.boundingBox = (xmin, ymin, xmax, ymax)


    def update(self):
        next = set()
        xmin, ymin, xmax, ymax = (0,0,0,0)

        for (x,y) in self.frontier():
            count = len(self.livingNeighbours(x,y))
            living = (x,y) in self.livingCells

            if (living and count in [2,3]) or (not living and count == 3):
                if x < xmin: xmin = x
                if x > xmax: xmax = x
                if y < ymin: ymin = y
                if y > ymax: ymax = y

                next.add((x,y))

        self.livingCells = next
        self.boundingBox = (xmin, ymin, xmax, ymax)

    def frontier(self):
        frontier = set()
        for (x,y) in self.livingCells:
            frontier.add((x,y))
            for (nx,ny) in self.adjacent(x,y):
                frontier.add((nx,ny))
        return frontier

    def adjacent(self, x, y):
        return [(nx,ny) for nx in range(x-1,x+2) for ny in range(y-1,y+2) if (nx,ny) != (x,y)]

    def livingNeighbours(self, x, y):
        return [z for z in self.adjacent(x,y) if z in self.livingCells]


    def __str__(self):
        return str(self.livingCells)


