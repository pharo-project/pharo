running
check: anAST forCritiquesDo: aCriticBlock ifNone: alternativeBlock

	| encounteredCritique |
	encounteredCritique := false.

	anAST nodesDo: [ :node |
		(node isVariable and: [
		node binding ifNil: [ node doSemanticAnalysis ].
		node isGlobal and: [ 
		self systemClassNames includes: node name ] ])
			ifTrue: [
				encounteredCritique := true.
				aCriticBlock cull: (ReTrivialCritique
					withAnchor: (ReIntervalSourceAnchor
						entity: anAST
						interval: node sourceInterval)
					by: self) ] ].

	encounteredCritique ifFalse: alternativeBlock	