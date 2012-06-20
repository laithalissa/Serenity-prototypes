__author__ = 'joseph'
from Tkinter import *
from game.Map import Map

class Window:

    ups = 5
    player1_color = "blue"
    player2_color = "red"
    background_color = "green"

    STOPPED = 0
    RUNNING = 1



    def __init__(self, world):
        self.world = world

        self.state = self.STOPPED
        self.updateTime = 1000/self.ups
        #self.updateTime = 3000
        self.root = Tk()
        self.root.config(borderwidth=0)
        self.root.wm_title("Prototype 1")

        self.frame = Frame(self.root, width=500, height=700, background="white")
        self.frame.pack()

        self.initMenu()

        self.canvas = Canvas(self.frame, width=500, height=500, background=self.background_color)
        self.canvas.pack()

        self.initCommands()

        self.canvasItems = []

        self.draw()

        self.viewPort = (1, 1, self.world.map.width, self.world.map.height)

    def initMenu(self):
        self.menuBar = Menu(self.root)
        self.root.config(menu=self.menuBar)

        fileMenu = Menu(self.menuBar, tearoff=0)
        self.menuBar.add_command(label="Exit", command=exit)
        self.menuBar.add_cascade(label="File", menu=fileMenu)


    def initCommands(self):
        self.commandsFrame = Frame(self.frame, width=500, height=100, background="yellow")
        self.commandsFrame.pack()

        self.timeSlider = Scale(self.commandsFrame,from_=self.world.timeLine.MIN_TIME,to=self.world.timeLine.MAX_TIME, orient=HORIZONTAL, length=500)
        self.timeSlider.pack()

    def update(self):
        """updates world and refreshes graphics"""
        if self.state == self.RUNNING:
            self.world.update()
        self.draw()
        self.updateGUI()
        self.root.after(self.updateTime, self.update)

    def updateGUI(self):
        self.timeSlider.config(to=self.world.timeLine.MAX_TIME)
        self.timeSlider.set(self.world.timeLine.getTimeNumeric())



    def draw(self):
        """draws the world's current time"""
        [self.canvas.delete(itemId) for itemId in self.canvasItems]

        canvasWidth, canvasHeight = self.canvas.winfo_width(), self.canvas.winfo_height()
        worldWidth, worldHeight = self.world.map.width, self.world.map.height
        xScale, yScale = float(canvasWidth) / float(worldWidth), float(canvasHeight) / float(worldHeight)

        soldiers = self.world.getSoldiers()

        for y in range(worldHeight, 0, -1):
            for x in range(1, worldWidth+1):
                if (x,y) in soldiers:
                    soldier = soldiers[(x,y)]
                    ax, ay = int((x-1)*xScale), int((worldHeight-y)*yScale)
                    bx, by = int(ax+xScale), int(ay+yScale)
                    color = self.player1_color if soldier.player == Map.P1 else self.player2_color
                    self.canvasItems.append(self.canvas.create_rectangle(ax,ay,bx,by,fill=color))


    def start(self):
        """starts the update loop and tkinter's mainloop"""
        #self.state = self.RUNNING
        self.root.after(self.updateTime, self.update)
        self.root.mainloop()