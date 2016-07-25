accessing
current
	^ (self index <= self size and: [self index > 0])
		ifTrue: [self recorder at: self index]