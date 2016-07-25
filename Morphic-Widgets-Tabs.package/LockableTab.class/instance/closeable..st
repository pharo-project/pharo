accessing
closeable: anObject

	closeable := anObject.
	anObject 
		ifTrue: [ self setUnlockedIcon ]
		ifFalse: [ self setLockedIcon ]