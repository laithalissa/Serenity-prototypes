from prototype.game.Soldier import Soldier

__author__ = 'joseph'

class Time:

    SOLDIER_KEEP_ATTACKING_UNTIL_VICTORY = False

    def __init__(self,time, soldiers, orders):
        self.time = time
        self.soldiers = soldiers
        self.orders = orders
        self.additionalOrders = []


    # could be refactored into multiple methods, one for each scenario
    def generateNextTime(self):
        """ generates the next time
        additionalOrders are new orders the user has specified.
        combines both existing orders and additional orders, deleting existing orders if they conflict with new orders.
        this is where orders are executed
        if a soldier is soldiers path is blocked any other soldier, order is cancelled.
        if soldier is ordered to adjacent square with enemy, soldier attacks enemy.
        if soldier is ordered to adjacent square with friendly soldier, order cancelled.
        """
        orders = dict((order.soldier.id, order.copy()) for order in self.additionalOrders)
        self.additionalOrders = []
        for id,order in self.orders.items():
            if id not in orders:
                orders[id] = order.copy()


        nextSoldiers = dict()
        nextOrders = dict()


        soldiersWithoutOrders = [(loc,soldier.copy()) for loc,soldier in self.soldiers.items() if soldier.id not in orders]
        soldiersWithOrders = [(loc,soldier.copy(),orders[soldier.id].copy()) for loc,soldier in self.soldiers.items() if soldier.id in orders]

        # adds all soldiers that don't have orders first
        for (x,y), soldier in soldiersWithoutOrders:
            nextSoldiers[(x,y)] = soldier

        for (x,y), soldier, order in soldiersWithOrders:

            """
                if next square empty => move to next square
                if next square contains ally => cancel order
                if next square contains enemy and can attack => attack
                if next square contains enemy and can't attack => cancel order
            """

            nextLocation = order.nextLocation()

            # can move freely
            if nextLocation not in nextSoldiers:
                order.move()
                nextSoldiers[nextLocation] = soldier
                if not order.finished():
                    nextOrders[soldier.id] = order

            # friendly soldier
            elif nextSoldiers[nextLocation].player == soldier.player:
                print "friendly at %s" % str(nextLocation)
                if (x,y) in nextSoldiers:
                    raise Exception("bug in design, another soldier taken existing soldier's place")
                else:
                    nextSoldiers[(x,y)] = soldier

            # enemy soldier and can attack
            elif nextSoldiers[nextLocation].player != soldier.player and order.attack:
                enemy = nextSoldiers[nextLocation]
                enemy.health -= soldier.damage
                print "enemy at %s belonging to %d has %d health" % (str(nextLocation),enemy.player, enemy.health)
                if enemy.health < enemy.MIN_HEALTH:
                    del nextSoldiers[nextLocation]
                    if enemy.id in nextOrders:
                        del nextOrders[enemy.id]
                    nextSoldiers[nextLocation] = soldier
                    order.move()
                    if not order.finished():
                        nextOrders[soldier.id] = order
                else:
                    # soldier stays put
                    if (x,y) in nextSoldiers:
                        raise Exception("bug in design, another soldier taken existing soldier's place")
                    else:
                        nextSoldiers[(x,y)] = soldier
                        if self.SOLDIER_KEEP_ATTACKING_UNTIL_VICTORY:
                            nextOrders[soldier.id] = order

            # enemy soldier and can't attack
            elif nextSoldiers[nextLocation].player != soldier.player and not order.attack:
                if (x,y) in nextSoldiers:
                    raise Exception("bug in design, another soldier taken existing soldier's place")
                else:
                    nextSoldiers[(x,y)] = soldier

            else:
                raise Exception("illegal state")

        t = Time(self.time+1, nextSoldiers, nextOrders)
        return t


    def addOrder(self, order):
        self.additionalOrders.append(order)

    def getSoldierByLocation(self,x,y):
        return self.soldiers[(x,y)].copy() if (x,y) in self.soldiers else None

    def getLocationById(self, id):
        for (location,soldier) in self.soldiers.items():
            if soldier.id == id:
                return location
        return None

    def getSoldierById(self, id):
        for soldier in self.soldiers.values():
            if soldier.id == id:
                return soldier.copy()
        return None

    def getOrderById(self, id):
        return self.orders[id].copy() if id in self.orders else None

    def __repr__(self):
        return str(self)

    def __str__(self):
        return "%s\ntime=%d,\nsoldiers=%s,\norder=%s]\n" % (self.__class__.__name__,self.time, str(list(self.soldiers)), str(list(self.orders.values())))








