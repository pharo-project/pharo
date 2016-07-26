visitor-double dispatching
visitReturnNode: aReturnNode 

	valueTranslator visitNode: aReturnNode value.
	self emitMetaLinkBefore: aReturnNode.
	methodBuilder returnTop.
		
