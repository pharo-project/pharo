searching
matchesArgumentTree: aBRProgramNode do: aBlock 
	self 
		addArgumentRule: (RBSearchRule searchForTree: aBRProgramNode thenDo: aBlock)