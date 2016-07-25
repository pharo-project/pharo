actions
checkSize

	| result |
	result := false.
	self maxListSize > 0 "0 means infinite"
		ifTrue: [
			result := (self methodReferenceList size > self maxListSize).
			[self methodReferenceList size > self maxListSize]
				whileTrue: [self methodReferenceList removeFirst]].
	^result