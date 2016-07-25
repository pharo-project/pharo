rotate scale and flex
privateMoveBy: delta
	self setProperty: #referencePosition toValue: self referencePosition + delta.
	self setProperty: #originalCenter toValue: (self valueOfProperty: #originalCenter ifAbsent: [ self center ]) + delta.
	super privateMoveBy: delta.
