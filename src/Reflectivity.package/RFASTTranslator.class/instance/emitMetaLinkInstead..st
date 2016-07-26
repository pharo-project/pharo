reflectivity
emitMetaLinkInstead: aNode
	self emitPreamble: aNode.
	valueTranslator visitNode: aNode insteadHooks.