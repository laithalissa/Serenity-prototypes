__author__ = 'joseph'
from Soldier import Soldier

class Map:


    BB = 0
    P1 = 1
    P2 = 2

    SMALL = [
        [BB, BB, BB, BB],
        [P1, BB, BB, P2],
        [BB, BB, BB, BB]
    ]

    MEDIUM = [
        [BB, P1, BB, BB, BB, P2, BB],
        [BB, P1, BB, BB, BB, P2, BB],
        [BB, P1, BB, BB, BB, P2, BB],
        [P1, P1, BB, BB, BB, P2, P2],
        [BB, P1, BB, BB, BB, P2, BB],
        [BB, P1, BB, BB, BB, P2, BB],
        [BB, P1, BB, BB, BB, P2, BB],
    ]

    def __init__(self,data):

        self.soldiers = dict()
        self.height = len(data)
        self.width = len(data[0])

        for rowIndex in range(self.height):
            for colIndex in range(self.width):
                x = colIndex + 1
                y = self.height - rowIndex
                val = data[rowIndex][colIndex]

                if val in [self.P1, self.P2]:
                    self.soldiers[(x,y)] = Soldier(val)

    def __getitem__(self, loc):
        return self.soldiers(loc) if loc in self.soldiers else None

    def __contains__(self, loc):
        return loc in self.soldiers


    def __str__(self):
        board = [ [self.soldiers[(x,y)] if (x,y) in self.soldiers else None for x in range(1,self.width+1)] for y in range(self.height,0,-1)]

        return "\n".join([" | ".join([ (str(soldier) if soldier else "").ljust(20) for soldier in row]) for row in board])






