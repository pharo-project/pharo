composition
addOnTheLeft: aTrait 
	self errorIfNotAddable: aTrait.
	self transformations addFirst: aTrait