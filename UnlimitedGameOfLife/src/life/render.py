__author__ = 'joseph'

from Tkinter import *
from life.handler import Handler
from time import time

class Render:

    STOPPED = 0
    RUNNING = 1

    UPS_MAX = 20
    UPS_MIN = 1

    VIEW_PORT_SIZE_MIN = 9
    VIEW_PORT_SIZE_MAX = 200




    def __init__(self, world):
        self.world = world
        self.root = Tk()
        self.canvas = Canvas(self.root, width=800, height=800,background="black")
        self.canvas.pack()
        self.canvasItems = []
        self.viewPort = (0,0,20,20)
        self.ups = 5
        self.fps = 5
        self.aups = 0
        self.state = self.STOPPED
        self.handler = Handler(self, world)
        self.draw()
        self.root.after(1000/self.ups, self.update)

    def update(self):
        start = time()
        if self.state == self.RUNNING:
            self.world.update()
        self.draw()
        duration = (time() - start) * 1000
        self.aups = int(1000.0 / duration)
        self.root.after(1000/self.ups, self.update)



    def draw(self):

        self.updateTitle()

        for itemId in self.canvasItems:
            self.canvas.delete(itemId)

        frameWidth = self.canvas.winfo_width()
        frameHeight = self.canvas.winfo_height()

        board = self.getCanvasBoard()
        boardHeight = len(board)
        boardWidth = len(board[0])

        xScale = float(frameWidth) / float(boardWidth)
        yScale = float(frameHeight) / float(boardHeight)

        for rowIndex in range(boardHeight):
            for colIndex in range(boardWidth):
                if board[rowIndex][colIndex]:
                    x1, y1 = colIndex * xScale, rowIndex * yScale
                    x2, y2 = (colIndex+1) * xScale, (rowIndex+1) * yScale

                    self.canvasItems.append(self.canvas.create_rectangle(x1, y1, x2, y2, fill='green'))


    def updateTitle(self):
        cellCount = len(self.world.livingCells)
        x, y, w, h = self.viewPort
        ups = self.ups
        aups = self.aups

        title = "Game of Life (cells:%d, ups:%d, aups: %d, x:%d, y:%d, width:%d, height:%d)" % (cellCount, ups,aups, x, y, w, h)
        self.root.wm_title(title)


    def getCanvasBoard(self):

        # [ [col 1] [col 2] [col 3] ]
        board = self.getVisibleBoard()
        for column in board:
            column.reverse()

        width = len(board)
        height = len(board[0])

        rows = [[board[colIndex][rowIndex] for colIndex in range(width)] for rowIndex in range(height)]
        return rows


    def getVisibleBoard(self):
        x, y, width, height = self.viewPort
        board = [[(xx,yy) in self.world.livingCells for yy in range(y,y+height)] for xx in range(x,x+width)]
        return board



    def start(self):
        self.handler.handle_h()
        self.state = self.RUNNING
        self.root.mainloop()
