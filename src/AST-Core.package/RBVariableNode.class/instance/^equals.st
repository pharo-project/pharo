comparing
= anObject 
	self == anObject ifTrue: [^true].
	((anObject isKindOf: self class) or: [self isKindOf: anObject class]) 
		ifFalse: [^false].
	^self name = anObject name