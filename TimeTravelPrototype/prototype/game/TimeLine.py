__author__ = 'joseph'

from Time import Time


class TimeLine:

    MIN_TIME = 1
    MAX_TIME = 100

    KEEP_FUTURE_ORDERS = False


    def __init__(self, map):
        self.map = map
        soldiers = dict((loc, self.map.soldiers[loc].copy()) for loc in self.map.soldiers)
        id = 1
        for soldier in soldiers.values():
            soldier.id = id
            id += 1

        self.times = [Time(self.MIN_TIME, soldiers, dict())]

    def goToTime(self, time):

        diff = time - self.getTimeNumeric()

        if diff > 0:
            while diff > 0:
                self.times.append(self.getCurrentTimeState().generateNextTime())
                diff-=1

        elif diff < 0:
            self.times = self.times[:diff]
            diff = 0

        self.timeLimitCheck()



    def timeLimitCheck(self):
        # updates the maximum value of the time-line if the current time approaches
        if self.MAX_TIME * 0.8 < self.getTimeNumeric():
            self.MAX_TIME *= 1.2

    def getCurrentState(self):
        time = self.getCurrentTimeState()
        soldiers = dict((loc, soldier.copy()) for (loc, soldier) in time.soldiers.items() )
        orders = dict((soldierId,order.copy()) for (soldierId, order) in time.orders.items() )
        return (soldiers, orders)


    def getCurrentTimeState(self):
        return self.times[-1]

    def getTimeNumeric(self):
        return len(self.times)