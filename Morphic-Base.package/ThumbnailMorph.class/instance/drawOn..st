drawing
drawOn: aCanvas
	"Draw a small view of a morph in another place. Guard against infinite recursion if that morph has a thumbnail of itself inside."
	| time |
	time := Time millisecondClockValue.
	self drawMeOn: aCanvas.
	drawTime := Time millisecondClockValue - time.
	drawTime < 0 ifTrue:[drawTime := nil].
