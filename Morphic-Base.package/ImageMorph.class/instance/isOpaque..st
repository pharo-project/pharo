accessing
isOpaque: aBool
	"Mark the receiver as being completely opaque or not"
	
	aBool == false
		ifTrue: [self removeProperty: #isOpaque]
		ifFalse: [self setProperty: #isOpaque toValue: aBool].
	self changed