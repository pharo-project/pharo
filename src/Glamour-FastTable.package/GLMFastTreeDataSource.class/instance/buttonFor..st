accessing
buttonFor: item
	^ ( (self canDisplayChildrenOf: item) and: [self hasChildren: item])
		ifTrue: [ item generateButton ]
		ifFalse: [ self class emptyMorph ]