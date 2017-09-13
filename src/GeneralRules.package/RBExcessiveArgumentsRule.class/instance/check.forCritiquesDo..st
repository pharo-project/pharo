running
check: aMethod forCritiquesDo: aCritiqueBlock
	(aMethod numArgs >= self argumentsCount) ifTrue: [
        aCritiqueBlock cull: (self critiqueFor: aMethod) ]
		