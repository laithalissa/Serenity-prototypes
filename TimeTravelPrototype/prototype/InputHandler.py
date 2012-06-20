__author__ = 'joseph'

class InputHandler:

    charToName  = {" ": "space"}


    def __init__(self, world, window):
        self. world = world
        self.window = window

        self.lastClicked = None


        self.window.root.bind("<Key>", self.handle_keyboard)
        self.window.canvas.bind("<Button-1>", self.click)

        self.window.timeSlider.bind("<B1-Motion>",self.timeSliderClick)



    def timeSliderClick(self, event):
        self.timeSliderClicked = True
        self.world.changeTimeTo(self.window.timeSlider.get())




    def handle_keyboard(self, event):
        char = event.char

        funcName = "handle_%s" % (self.charToName[char] if char in self.charToName else char)
        if hasattr(self, funcName):
            getattr(self, funcName)()


    def handle_space(self):
        self.window.state = self.window.RUNNING if self.window.state == self.window.STOPPED else self.window.STOPPED

    def handle_d(self):
        self.window.state = self.window.STOPPED
        self.world.update()
        self.window.draw()
        self.window.updateGUI()

    def handle_a(self):
        self.window.state = self.window.STOPPED
        self.world.changeTimeTo(self.world.timeLine.getTimeNumeric()-1)
        self.window.draw()
        self.window.updateGUI()

    def click(self, event):
        x, y = event.x, event.y
        frameWidth = self.window.canvas.winfo_width()
        frameHeight = self.window.canvas.winfo_height()
        vx, vy, vw, vh = self.window.viewPort

        frameX = x
        frameY = frameHeight - y + 1

        xScale = float(vw) / float(frameWidth)
        yScale = float(vh) / float(frameHeight)

        viewPortX = int(frameX * xScale)
        viewPortY = int(frameY * yScale)

        location = (vx + viewPortX, vy + viewPortY)

        if self.lastClicked == None:
            if location in self.world.timeLine.getCurrentState()[0]:
                self.lastClicked = location
        else:
            self.world.order(self.lastClicked, location)
            self.lastClicked = None








