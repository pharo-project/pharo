event handling
mouseDown: evt

	| pos |
	
	pos := evt cursorPoint.
	
	"left button"
	evt redButtonPressed ifTrue: [ 
		self setState: #panning value: { transform x@transform y. pos }
		 ].
	
