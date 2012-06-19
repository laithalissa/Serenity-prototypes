__author__ = 'joseph'

from Time import Time


class TimeLine:

    MIN_TIME = 1
    MAX_TIME = 1000


    def __init__(self, map):
        self.map = map
        soldiers = dict((loc, self.map.soldiers[loc].copy()) for loc in self.map.soldiers)
        id = 1
        for soldier in soldiers.values():
            soldier.id = id
            id += 1

        self.times = [Time(self.MIN_TIME, soldiers, dict())]

    def incrementTime(self):
        # TODO accept inputs
        self.times.append(self.getCurrentTimeState().generateNextTime())

    def goToTime(self, time):
        print "changing the time, %d, %d" % (time, self.getTimeNumeric())
        if time - 1 == self.getTimeNumeric():
            self.incrementTime()
            print "incrementing"



    def getCurrentState(self):
        time = self.getCurrentTimeState()
        soldiers = dict((loc, soldier.copy()) for (loc, soldier) in time.soldiers.items() )
        orders = dict((soldierId,order.copy()) for (soldierId, order) in time.orders.items() )
        return (soldiers, orders)


    def getCurrentTimeState(self):
        return self.times[-1]

    def getTimeNumeric(self):
        return len(self.times)