__author__ = 'joseph'
from array import array

from life.world import World
from life.render import Render



world = World(10)

world.setCell(3,3)
world.setCell(2,2)
world.setCell(4,3)
world.setCell(4,2)
world.setCell(3,4)


render = Render(world)
render.start()