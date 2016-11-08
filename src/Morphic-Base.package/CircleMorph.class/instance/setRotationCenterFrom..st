menus
setRotationCenterFrom: aPoint
	"Called by halo rotation code.
	Circles store their referencePosition."

	referencePosition := aPoint.
	originalCenter := self center.
	originalAngle := self heading