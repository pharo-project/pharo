accessing
prevIfNone: aBlock
	
	^ prev == self ifTrue: [ aBlock value] ifFalse: [ prev ]