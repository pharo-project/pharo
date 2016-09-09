running
check: aClass forCritiquesDo: aCriticBlock
	
	aClass instVarNames
		select: [ :each | each first isUppercase ]
		thenDo: [ :each |
			aCriticBlock cull: (
				self critiqueFor: aClass about: each) ]