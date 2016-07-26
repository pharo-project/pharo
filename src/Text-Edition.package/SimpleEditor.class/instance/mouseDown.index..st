events
mouseDown: evt index: index
	
	self closeTypeIn.
	evt shiftPressed
		ifFalse: [
			self pointIndex: index.
			self markIndex: index]
		ifTrue: [
			self mouseMove: evt ]