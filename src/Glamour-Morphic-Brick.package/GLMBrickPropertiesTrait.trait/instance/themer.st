brick-properties
themer

	^ self privateThemer
		ifNotNil: [ self privateThemer ]
		ifNil: [ self hasOwner
			ifTrue: [ self owner themer ]
			ifFalse: [ self theme brickThemer ] ]