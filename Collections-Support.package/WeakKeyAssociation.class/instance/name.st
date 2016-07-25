accessing
name

	^ self key isString
		ifTrue: [self key]
		ifFalse: [self key printString]