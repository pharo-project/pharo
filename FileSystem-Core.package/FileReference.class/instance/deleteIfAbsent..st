operations
deleteIfAbsent: aBlock
	self exists 
		ifTrue: [ self delete ]
		ifFalse: aBlock