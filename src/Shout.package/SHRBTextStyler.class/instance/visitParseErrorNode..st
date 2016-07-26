visiting rb nodes
visitParseErrorNode: anErrorNode
	self 
		addStyle: #invalid
		forNode: anErrorNode