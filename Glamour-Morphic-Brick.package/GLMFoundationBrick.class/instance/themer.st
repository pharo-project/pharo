accessing
themer

	^ self privateThemer
		ifNotNil: [ self privateThemer ]
		ifNil: [ self owner isNotNil
			ifTrue: [ self owner isBrick ifTrue: [ self owner themer ] ifFalse: [ super brickThemer ] ]
			ifFalse: [ self theme brickThemer ] ]