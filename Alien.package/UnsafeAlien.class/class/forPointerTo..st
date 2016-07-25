instance creation
forPointerTo: anObject
	(anObject class instSpec between: 6 and: 8) ifFalse:
		[^self error: 'only use UnsafeAlien on instances of isBits classes (other than CompiledMethod)!'].
	^self new nonPointerObject: anObject