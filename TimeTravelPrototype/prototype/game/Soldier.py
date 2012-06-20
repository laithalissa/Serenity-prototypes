__author__ = 'joseph'

class Soldier:

    MAX_HEALTH = 100
    MIN_HEALTH = 1

    def __init__(self, player):
        self.player = player
        self.id = 0
        self.health = Soldier.MAX_HEALTH
        self.damage = 50


    def copy(self):
        c = Soldier(self.player)
        c.id = self.id
        c.health = self.health
        c.damage = self.damage
        return c



    def __repr__(self):
        return "%s(%s,%s,%s)" % (self.__class__.__name__, str(self.player), str(self.health), str(self.damage))

