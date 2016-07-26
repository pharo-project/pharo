stepping
stepTime
	^ model
		ifNotNil: [model stepTimeIn: self]
		ifNil: [200] "milliseconds"