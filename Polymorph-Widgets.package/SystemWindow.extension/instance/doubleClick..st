*Polymorph-Widgets
doubleClick: event
	"Handle a double click. Maximize/restore the window.
	Works in title bar area."

	self isResizeable ifFalse: [ ^ self ].
	
	(labelArea containsPoint: event position)
		ifTrue: [self expandBoxHit]