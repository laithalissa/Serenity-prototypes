__author__ = 'joseph'

class Order:
    """an order given to a soldier"""

    def __init__(self, soldier, startLocation, finishLocation,**args):
        print args
        self.soldier = soldier
        self.location = args["location"] if "location" in args else startLocation
        self.startLocation = startLocation
        self.finishLocation = finishLocation
        self.steps = args["steps"] if "steps" in args else self.route()
        self.attack = args["attack"] if "attack" in args else (1 == self.distance(startLocation, finishLocation))


    def route(self):
        sx, sy = self.startLocation
        fx, fy = self.finishLocation
        diffX, diffY = fx - sx, fy - sy
        xUnit = diffX / abs(diffX) if diffX != 0 else 0
        yUnit = diffY / abs(diffY) if diffY != 0 else 0

        steps = []
        while diffX != 0 or diffY != 0:
            if diffX != 0:
                diffX-= xUnit
                steps.append((xUnit, 0))
            if diffY != 0:
                diffY -= yUnit
                steps.append((0, yUnit))
        return steps


    def distance(self, p1, p2):
        x1, y1 = p1
        x2, y2 = p2
        return int(((x2-x1)**2 + (y2-y1)**2)**0.5)

    def finished(self):
        return len(self.steps) == 0

    def currentLocation(self):
        return self.location

    def nextLocation(self):
        x, y = self.location
        dx, dy = self.steps[0]
        return (x+dx, y+dy)

    def move(self):
        self.location = self.nextLocation()
        del self.steps[0]
        return self.location


    def copy(self):
        return Order(self.soldier.copy(), self.startLocation, self.finishLocation, location=self.location, steps=list(self.steps), attack=self.attack)

    def __repr__(self):
        return str(self)

    def __str__(self):
        soldierId = self.soldier.id
        dummyOrder = self.copy()
        globalSteps = [dummyOrder.currentLocation()]
        while not dummyOrder.finished():
            globalSteps.append(dummyOrder.move())
        return "Order[id=%d, steps=%s]" % (soldierId, " -> ".join([str(x) for x in globalSteps]))

