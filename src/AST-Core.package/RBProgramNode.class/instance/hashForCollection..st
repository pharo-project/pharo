comparing
hashForCollection: aCollection
	^ aCollection isEmpty ifTrue: [ 0 ] ifFalse: [ aCollection first hash ]