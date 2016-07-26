operation
insertLineSeparator: separator at: position 

	position spanPosition > 0 ifTrue: [
		self insertNext: separator. 
		^ next endPosition ].
	
	self insertPrev: separator.
	^ self startPosition