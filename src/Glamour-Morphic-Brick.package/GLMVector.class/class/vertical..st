instance creation
vertical: aNumber
	^ aNumber >= 0
		ifTrue: [ self south: aNumber ]
		ifFalse: [ self north: aNumber abs ]