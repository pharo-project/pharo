event
mouseMove: anEvent
	"Track the mouse for resizing."
	
	target ifNil: [^ self].
	target fastFramingOn 
		ifTrue: [target doFastWindowReframe: self ptName] 
		ifFalse: [
			lastMouse at: 1 put: anEvent cursorPoint.
			self targetPoint: lastMouse first - lastMouse last.
			self position: (lastMouse first - lastMouse second)].