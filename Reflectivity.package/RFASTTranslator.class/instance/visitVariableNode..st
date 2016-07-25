visitor-double dispatching
visitVariableNode: aVariableNode
	self emitMetaLinkBefore: aVariableNode.
	aVariableNode hasMetalinkInstead
				ifTrue: [ self emitMetaLinkInstead: aVariableNode ]
				ifFalse: [ self visitVariableValue: aVariableNode binding variable].
	self emitMetaLinkAfterNoEnsure: aVariableNode.
	
	