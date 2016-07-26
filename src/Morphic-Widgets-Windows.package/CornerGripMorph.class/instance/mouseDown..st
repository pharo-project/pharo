event
mouseDown: anEvent
	"Remember the receiver and target offsets too."
	
	|cp|
	cp := anEvent cursorPoint.
	lastMouse := {cp. cp - self position. cp - self targetPoint}