running
check: aMethod forCritiquesDo: aCriticBlock
	(aMethod sourceCode includes: Character lf) ifTrue: [ 
		aCriticBlock cull: (self critiqueFor: aMethod) ]