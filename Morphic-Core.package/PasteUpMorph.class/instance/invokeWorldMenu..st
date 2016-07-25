world menu
invokeWorldMenu: evt
	"Put up the world menu, triggered by the passed-in event."
	| menu |
	self bringTopmostsToFront.
	"put up screen menu"
	(menu := self worldMenu) popUpEvent: evt in: self.
	^ menu