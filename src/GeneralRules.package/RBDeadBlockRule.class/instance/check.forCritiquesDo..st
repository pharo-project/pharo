as yet unclassified
check: node forCritiquesDo: aCriticBlock
	(node isBlock and: [ node isUsed not ])
		ifTrue: [ aCriticBlock cull: (self critiqueFor: node) ]