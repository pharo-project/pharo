visitor-double dispatching
visitAssignmentNode: anAssignmentNode 
	valueTranslator visitNode: anAssignmentNode value.
	self emitMetaLinkBefore: anAssignmentNode.
	self emitMetaLinkBefore: anAssignmentNode variable.
	

	anAssignmentNode hasMetalinkInstead
				ifTrue: [ self emitMetaLinkInstead: anAssignmentNode ]
				ifFalse: [
					 anAssignmentNode variable hasMetalinkInstead
						ifTrue: [ self emitMetaLinkInstead: anAssignmentNode variable]
						ifFalse: [ anAssignmentNode variable binding emitStore: methodBuilder]].
	self emitMetaLinkAfterNoEnsure: anAssignmentNode variable.
	self emitMetaLinkAfterNoEnsure: anAssignmentNode.
	