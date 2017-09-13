enumerating
check: aClass forCritiquesDo: aCritiqueBlock
	aClass isMeta ifTrue: [ ^ self ].
	
	aClass allSuperclasses size >= self inheritanceDepth ifTrue: [
        aCritiqueBlock cull: (self critiqueFor: aClass) ]