comparing
= anObject 
	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
	^self variable = anObject variable and: [self value = anObject value]