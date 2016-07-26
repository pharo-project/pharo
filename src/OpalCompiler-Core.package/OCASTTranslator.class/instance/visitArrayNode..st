visitor-double dispatching
visitArrayNode: anArrayNode 

	| elementNodes |
	
	anArrayNode statements size > 32 ifTrue: [^ self visitLargeArrayNode: anArrayNode ].
	
	elementNodes := anArrayNode children.
	elementNodes do: [:node | valueTranslator visitNode: node].
	methodBuilder pushConsArray: elementNodes size.