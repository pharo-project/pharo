formatting
addStyle: styleSymbol attributes: additionalAttributes forNode: anRBNode
	self 
		addAttributes: (self attributesFor: styleSymbol), additionalAttributes 
		forNode: anRBNode.