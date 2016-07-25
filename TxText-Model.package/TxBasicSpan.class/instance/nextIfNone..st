accessing
nextIfNone: aBlock
	
	^ next == self ifTrue: [ aBlock value] ifFalse: [ next ]