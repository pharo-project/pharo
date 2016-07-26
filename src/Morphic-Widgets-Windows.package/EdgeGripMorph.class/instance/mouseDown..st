event handling
mouseDown: anEvent
	"Remember the receiver and target offsets too."
	
	|cp|
	(self bounds containsPoint: anEvent cursorPoint)
		ifTrue: [self fillStyle: self pressedFillStyle].
	cp := anEvent cursorPoint.
	lastMouse := {cp. cp - self position. cp - self targetPoint}.
	self eventHandler
		ifNotNil: [self eventHandler mouseDown: anEvent fromMorph: self]
