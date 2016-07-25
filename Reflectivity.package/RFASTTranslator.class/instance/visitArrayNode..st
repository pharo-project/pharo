visitor-double dispatching
visitArrayNode: anArrayNode 

	| elementNodes |
	
	anArrayNode statements size > 32 ifTrue: [^ self visitLargeArrayNode: anArrayNode ].
	
	elementNodes := anArrayNode children.
	elementNodes do: [:node | valueTranslator visitNode: node].
	self emitMetaLinkBefore: anArrayNode.
	anArrayNode hasMetalinkInstead
		ifTrue: [ self emitMetaLinkInstead: anArrayNode ]
		ifFalse: [ methodBuilder pushConsArray: elementNodes size ].
	self emitMetaLinkAfterNoEnsure: anArrayNode.
