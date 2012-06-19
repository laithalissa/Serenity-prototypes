__author__ = 'joseph'

from random import random
import tkMessageBox

class Handler:

    charToName = \
        { "+": "plus"
        , "-": "minus"
        , " ": "space"
        }

    def __init__(self, render, world):
        self.render = render
        self.world = world

        self.render.root.bind("<Key>", self.handle)
        self.render.canvas.bind("<Button-1>",self.click)

    def __str__(self):
        actions = {
            "+": "speed up game play",
            "-": "slow down game play",
            "space": "pauses/unpauses game play",
            "w/a/s/d": "moves the camera around the world",
            "q/e": "zooms in/out of the world",
            "r": "generates random cells within the view",
            "c": "clears all the cells on the board",
            "n": "takes the view to a living cell if it exists",
            "click": "clicking anywhere on the board will toggle the cell",
            "h": "show this help dialog",
        }

        return "\n".join(["%s: %s" % (key, value) for (key, value) in actions.items()])



    def viewPortChange(self,dx=0,dy=0,dw=0,dh=0):
        x,y,w,h = self.render.viewPort
        vpdx, vpdy = self.viewPortDelta()

        dx, dy, dw, dh = dx*vpdx, dy*vpdy, dw*vpdx, dh*vpdy

        if (self.render.VIEW_PORT_SIZE_MIN <= (w+dw) <= self.render.VIEW_PORT_SIZE_MAX)  \
            and (self.render.VIEW_PORT_SIZE_MIN <= (h+dh) <= self.render.VIEW_PORT_SIZE_MAX):

                self.render.viewPort = (x+dx,y+dy,w+dw,h+dh)


    def viewPortDelta(self):
        x,y,w,h = self.render.viewPort
        return (max(1, w/10), max(1, h/10))

    def handle(self, event):
        char = event.char

        funcName = "handle_%s" % (self.charToName[char] if char in self.charToName else char)
        if hasattr(self, funcName):
            getattr(self, funcName)()


    def handle_plus(self):
        """speeds up game speed"""
        if self.render.ups < self.render.UPS_MAX:
            self.render.ups+=1

        print "ups is %d" % self.render.ups

    def handle_minus(self):
        """slows down game speed"""
        if self.render.ups > self.render.UPS_MIN:
            self.render.ups-=1

        print "ups is %d" % self.render.ups

    def handle_space(self):
        """pauses/unpause game"""
        self.render.state = self.render.RUNNING if self.render.state == self.render.STOPPED else self.render.STOPPED

    def handle_w(self):
        self.viewPortChange(dy=1)

    def handle_s(self):
        self.viewPortChange(dy=-1)

    def handle_a(self):
        self.viewPortChange(dx=-1)

    def handle_d(self):
        self.viewPortChange(dx=1)

    def handle_q(self):
        self.viewPortChange(dx=1,dy=1,dw=-2,dh=-2)

    def handle_e(self):
        self.viewPortChange(dx=-1,dy=-1,dw=2,dh=2)

    def handle_r(self):
        """random generates cells in the view port"""
        vx, vy, vw, vh = self.render.viewPort
        alive_rate = 0.2
        for x in range(vx, vx+vw):
            for y in range(vy, vy+vh):
                self.world.setCell(x, y, random() < alive_rate)

    def handle_c(self):
        """clears the board"""
        self.world.livingCells.clear()

    def handle_n(self):
        """takes the viewport to a living cell"""
        vx, vy, vw, vh = self.render.viewPort

        if(len(self.world.livingCells) > 0):
            cx, cy = self.world.livingCells.__iter__().next()

            nx = cx - vw/2
            ny = cy - vh/2

            self.render.viewPort = (nx, ny, vw, vh)

    def handle_h(self):
        "displays help dialog"
        initial = self.render.state
        self.render.state = self.render.STOPPED
        tkMessageBox.showinfo(message="controls:\n%s" % str(self))
        self.render.state = initial





    def click(self, event):
        self.render.state = self.render.STOPPED
        x, y = event.x, event.y
        frameWidth = self.render.canvas.winfo_width()
        frameHeight = self.render.canvas.winfo_height()
        vx, vy, vw, vh = self.render.viewPort

        frameX = x
        frameY = frameHeight - y + 1

        xScale = float(vw) / float(frameWidth)
        yScale = float(vh) / float(frameHeight)

        viewPortX = int(frameX * xScale)
        viewPortY = int(frameY * yScale)

        worldX = vx + viewPortX
        worldY = vy + viewPortY

        self.world.setCell(worldX, worldY)




