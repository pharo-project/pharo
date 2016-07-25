visiting rb nodes
visitVariableNode: aVariableNode
	self 
		addStyle: (self resolveStyleFor: aVariableNode) 
		attribute: (self resolveTextLinkFor:aVariableNode)
		forNode: aVariableNode.
	
	