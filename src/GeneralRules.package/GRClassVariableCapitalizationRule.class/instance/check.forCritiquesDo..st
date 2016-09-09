running
check: aClass forCritiquesDo: aCriticBlock
	aClass isMeta ifTrue: [ ^ self ].
	aClass classVarNames, aClass sharedPoolNames
		select: [ :each | each first isLowercase ]
		thenDo: [ :each |
			aCriticBlock cull:
				(self critiqueFor: aClass about: each) ]