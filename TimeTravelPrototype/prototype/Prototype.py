__author__ = 'joseph'

from InputHandler import InputHandler
from Window import Window
from game.World import World
from game.Map import Map

class Prototype:

    def __init__(self):
        self.map = Map(Map.SMALL)
        self.world = World(self.map)
        self.window = Window(self.world)
        self.inputHandler = InputHandler(self.world, self.window)

    def start(self):
        self.window.start()


