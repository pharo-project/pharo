testing
literalEquality: x and: y
	"Check if two literals should be considered equal and reduced to a single literal.
	Delegate this task to the literal themselves, they are aware of their peculiarities and know how to behave."
	
	^ x literalEqual: y
