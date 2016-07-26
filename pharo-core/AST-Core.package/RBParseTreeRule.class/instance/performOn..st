matching
performOn: aProgramNode 
	self context empty.
	^((searchTree match: aProgramNode inContext: self context) 
		and: [self canMatch: aProgramNode]) 
			ifTrue:
				[owner recusivelySearchInContext.
				self foundMatchFor: aProgramNode]
			ifFalse: [nil]
			
		