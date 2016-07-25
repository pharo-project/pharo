methods
guardingClause
	self isSymbol 
		ifFalse: 
			[self printString.
			self isSymbol printString]