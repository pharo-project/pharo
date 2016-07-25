event handling
mouseMove: anEvent
	"Track the mouse for resizing."
	
	target ifNil: [^self].
	self theme settings fastDragging
		ifTrue: [target doFastReframe: self edgeName] 
		ifFalse: [
			lastMouse at: 1 put: anEvent cursorPoint.
			self targetPoint: lastMouse first - lastMouse last.
			self positionPoint: (lastMouse first - lastMouse second)].