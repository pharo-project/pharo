character writing
ensureNoSpace
	"If there is not one on the end, remove it."
	| space |
	
	space := Character space.
	
	[ position > 0 and: [(collection at: position) = space ]]
		whileTrue: [ self skip: -1 ]