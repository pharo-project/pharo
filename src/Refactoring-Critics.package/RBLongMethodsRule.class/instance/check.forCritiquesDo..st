running
check: aMethod forCritiquesDo: aCriticBlock
	| numberOfStatements |
	numberOfStatements := 0.

	(aMethod ast nodesDo: [ :node |
		node isSequence ifTrue: [ 
			numberOfStatements := numberOfStatements + node statements size.
			numberOfStatements >= self longMethodSize ifTrue: [
				^ aCriticBlock cull: (self critiqueFor: aMethod) ] ] ])
		