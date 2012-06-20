from prototype.game.Soldier import Soldier

__author__ = 'joseph'

class Time:

    SOLDIER_KEEP_ATTACKING_UNTIL_VICTORY = False

    def __init__(self,time, soldiers, orders):
        self.time = time
        self.soldiers = soldiers
        self.orders = orders
        self.additionalOrders = []


    def generateNextTime(self):
        """ generates the next time
        additionalOrders are new orders the user has specified.
        combines both existing orders and additional orders, deleting existing orders if they conflict with new orders.
        this is where orders are executed
        if a soldier is soldiers path is blocked any other soldier, order is cancelled.
        if soldier is ordered to adjacent square with enemy, soldier attacks enemy.
        if soldier is ordered to adjacent square with friendly soldier, order cancelled.
        """
        additionalOrders = dict((order.soldier.id, order) for order in self.additionalOrders)
        self.additionalOrders = []

        nextSoldiers = dict()
        nextOrders = dict()
        for (x,y), soldier in self.soldiers.items():
            order = None
            if soldier.id in additionalOrders:
                order = additionalOrders[soldier.id]
            elif soldier.id in self.orders:
                order = self.orders[soldier.id]


            if order == None:
                nextSoldiers[(x,y)] = soldier.copy()
            # assumes no orders are finished
            else:
                """
                    if next square empty => move to next square
                    if next square contains ally => cancel order
                    if next square contains enemy and can attack => attack
                    if next square contains enemy and can't attack => cancel order
                """

                # integrity check
                if order.soldier.id != soldier.id:
                    raise Exception("illegal state")

                nextLocation = order.nextLocation()
                nextSoldier = soldier.copy()
                nextOrder = order.copy()

                # can move freely
                if nextLocation not in nextSoldiers:
                    nextOrder.move()
                    nextSoldiers[(nextLocation)] = nextSoldier
                    if not nextOrder.finished():
                        nextOrders[nextSoldier.id] = nextOrder



                # another soldier is at the next location, cancel order
                # friendly soldier
                elif nextSoldiers[nextLocation].player == nextSoldier.player:
                    if (x,y) in nextSoldiers:
                        raise Exception("bug in design, another soldier taken existing soldier's place")
                    else:
                        nextSoldiers[(x,y)] = nextSoldier

                # enemy soldier and can attack
                elif nextSoldiers[nextLocation].player != nextSoldier.player and nextOrder.attack:

                    enemy = nextSoldiers[nextLocation]
                    enemy.health -= nextSoldier.damage
                    print "enemy at %s belonging to %d has %d health" % (str(nextLocation),enemy.player, enemy.health)
                    if enemy.health < Soldier.MIN_HEALTH:
                        del nextSoldiers[nextLocation]
                        if enemy.id in nextOrders:
                            del nextOrders[enemy.id]
                        nextSoldiers[nextLocation] = nextSoldier
                        nextOrder.move()
                        if not nextOrder.finished():
                            nextOrders[nextSoldier.id] = nextOrder

                    else:
                        # soldier stays put
                        if (x,y) in nextSoldiers:
                            raise Exception("bug in design, another soldier taken existing soldier's place")
                        else:
                            nextSoldiers[(x,y)] = nextSoldier
                            if self.SOLDIER_KEEP_ATTACKING_UNTIL_VICTORY:
                                nextOrders[nextSoldier.id] = nextOrder

                # enemy soldier and can't attack
                elif nextSoldiers[nextLocation].player != nextSoldier.player and not nextOrder.attack:
                    if (x,y) in nextSoldiers:
                        raise Exception("bug in design, another soldier taken existing soldier's place")
                    else:
                        nextSoldiers[(x,y)] = nextSoldier

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








