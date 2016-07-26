path commands
moveTo: aPoint
	" move command always starts a new contour "
	contourStartPt := self toAbsolute: aPoint.
	
	"collapse multiple moves to a single one"
	lastSegment isMove ifTrue: [
		lastSegment point: contourStartPt.
		^ self
	].

	self addSegment: (AthensMoveSegment new point: contourStartPt; reopen: open).
	open := true.