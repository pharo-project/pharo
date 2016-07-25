accessing
layoutPosition: aPosition
	
	lastChangeTick := Time millisecondClockValue.
	layoutPosition := aPosition.

	"update bounds according to new layout position"
	self bounds: self calculateNewBounds.
	
"	self startStepping"