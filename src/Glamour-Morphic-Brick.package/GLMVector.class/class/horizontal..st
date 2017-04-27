instance creation
horizontal: aNumber
	^ aNumber >= 0
		ifTrue: [ self east: aNumber ]
		ifFalse: [ self west: aNumber abs ]