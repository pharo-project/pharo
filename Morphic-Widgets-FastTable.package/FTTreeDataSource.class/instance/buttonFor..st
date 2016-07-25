accessing
buttonFor: item
	^ ( (self canDisplayChildrenOf: item) and: [item children isNotEmpty])
		ifTrue: [ item generateButton ]
		ifFalse: [ self class emptyMorph ]