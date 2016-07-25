visiting
visitLiteralNode: node 
	
	self 
		addStyle: (self literalStyleSymbol: node value)
		forNode: node