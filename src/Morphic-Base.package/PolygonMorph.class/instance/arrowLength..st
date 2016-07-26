menu
arrowLength: aLength
	"Assumes that I have exactly two vertices"

	| theta horizontalOffset verticalOffset newTip delta |
	delta := vertices second - vertices first.
	theta := delta theta.
	horizontalOffset := aLength * (theta cos).
	verticalOffset := aLength * (theta sin).
	newTip := vertices first + (horizontalOffset @ verticalOffset).
	self verticesAt: 2 put: newTip