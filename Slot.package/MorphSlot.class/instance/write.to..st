meta-object-protocol
write: aValue to: anObject
	
	(self read: anObject) ifNotNil: [ :m | m delete ].
	^super write: aValue to: anObject 