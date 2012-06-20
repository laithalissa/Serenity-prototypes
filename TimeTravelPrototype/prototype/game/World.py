__author__ = 'joseph'

from TimeLine import TimeLine
from Order import Order

class World:

    def __init__(self, map):
        self.map = map
        self.timeLine = TimeLine(map)

        #self.order((1,2), (4,1))


    def locationInWorld(self, location):
        x,y = location
        return (1 <= x <= self.map.width) and (1 <= y <= self.map.height)


    def order(self, startLocation, finishLocation):

        #checks both locations are not the same
        if startLocation == finishLocation:
            print "ordered to same location"
            return

        # checks first startLocation has a soldier
        if startLocation not in self.timeLine.getCurrentState()[0]:
            print "no soldier at %s" % str(startLocation)
            return

        # checks both locations are within the world boundary
        if not self.locationInWorld(startLocation) or not self.locationInWorld(finishLocation):
            print "location not in world"
            return

        time = self.timeLine.getCurrentTimeState()

        soldier = time.getSoldierByLocation(*startLocation)
        time.addOrder(Order(soldier, startLocation, finishLocation))


    def update(self):
        self.timeLine.goToTime(self.timeLine.getTimeNumeric()+1)

    def changeTimeTo(self, time):
        self.timeLine.goToTime(time)


    def getSoldiers(self):
        """returns a mapping from location to soldier"""
        return self.timeLine.getCurrentState()[0]

    def getOrders(self):
        """returns a mapping from soldier id to order"""
        return self.timeLine.getCurrentState()[1]
