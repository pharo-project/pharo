running
check: aMethod forCritiquesDo: aCriticBlock
	
	aMethod ast
		nodesDo: [ :each | 
			(each isSequence and:
			[ each periods size > each statements size ])
				ifTrue: [ aCriticBlock cull: (self critiqueFor: aMethod) ] ]