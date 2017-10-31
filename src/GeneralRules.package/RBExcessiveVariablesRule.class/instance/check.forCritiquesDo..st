running
check: aClass forCritiquesDo: aCritiqueBlock
	(aClass instVarNames size >= self variablesCount
		or: [ aClass classVarNames size >= self variablesCount ]) ifTrue: [
        aCritiqueBlock cull: (self critiqueFor: aClass) ]
			