menus
setRotationCenterFrom: aPoint
	"Called by halo rotation code.
	Circles store their referencePosition."
	self setProperty: #referencePosition toValue: aPoint.
	self setProperty: #originalCenter toValue: self center.
	self setProperty: #originalAngle toValue: self heading.