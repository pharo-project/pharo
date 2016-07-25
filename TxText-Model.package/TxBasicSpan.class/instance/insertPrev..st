accessing
insertPrev: aSpan
	
	self isAtStart 
		ifFalse: [ aSpan prev: prev ].
	aSpan next: self.
	^ aSpan
