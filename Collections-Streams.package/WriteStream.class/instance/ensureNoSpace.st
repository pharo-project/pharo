character writing
ensureNoSpace
	"If there is not one on the end, remove it."

	(position > 0 and: [(collection at: position) = Character space]) 
		ifTrue: [self skip: -1].