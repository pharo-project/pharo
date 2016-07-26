stepping and presenter
step
	self shouldCopy = dragHand shiftPressed
		ifTrue: [ ^ self ].
	
	self shouldCopy: dragHand shiftPressed.
	self updateCopyIcon.