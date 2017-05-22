printing
printShortTime
	^ self datetime asDate = Date today
	ifTrue: [ self datetime asTime asString ]
	ifFalse: [ self datetime asDate asString ]