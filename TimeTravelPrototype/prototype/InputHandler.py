__author__ = 'joseph'

class InputHandler:

    charToName  = {" ": "space"}

    def __init__(self, world, window):
        self. world = world
        self.window = window

        self.window.root.bind("<Key>", self.handle_keyboard)

    def handle_keyboard(self, event):
        char = event.char

        funcName = "handle_%s" % (self.charToName[char] if char in self.charToName else char)
        if hasattr(self, funcName):
            getattr(self, funcName)()


    def handle_space(self):
        print "spaced"
        self.window.state = self.window.RUNNING if self.window.state == self.window.STOPPED else self.window.STOPPED