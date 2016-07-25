comparing
beginsWith: prefix
	"Answer whether the receiver begins with the given prefix string.
	The comparison is case-sensitive."


	"IMPLEMENTATION NOTE:
	following algorithm is optimized in primitive only in case self and prefix are bytes like.
	Otherwise, if self is wide, then super outperforms,
	Otherwise, if prefix is wide, primitive is not correct"
	
	prefix class isBytes ifFalse: [^super beginsWith: prefix].
	
	self size < prefix size ifTrue: [^ false].
	^ (self findSubstring: prefix in: self startingAt: 1
			matchTable: CaseSensitiveOrder) = 1
