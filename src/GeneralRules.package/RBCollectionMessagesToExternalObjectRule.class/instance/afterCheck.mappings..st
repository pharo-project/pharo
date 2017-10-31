hooks
afterCheck: aNode mappings: mappingDict
	| collectionOwner |
	((mappingDict at: '`@collectionGetter:') beginsWith: 'as') ifTrue: [ ^ false ].
	collectionOwner := mappingDict at: (RBPatternVariableNode named: '`@collectionOwner').
	 collectionOwner isVariable ifFalse: [ ^ true ].
	^ self isNotSpecialVariable: collectionOwner.
	