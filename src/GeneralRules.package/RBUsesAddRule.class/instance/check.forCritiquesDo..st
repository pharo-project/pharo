as yet unclassified
check: node forCritiquesDo: aCriticBlock
	(node isMessage
		and: [ (node selector == #add: or: [ node selector == #addAll: ])
				and: [ node isDirectlyUsed ] ])
		ifTrue: [ aCriticBlock cull: (self critiqueFor: node) ]