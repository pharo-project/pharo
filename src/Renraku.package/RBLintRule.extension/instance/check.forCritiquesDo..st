*renraku
check: anEntity forCritiquesDo: aCriticBlock

	(self basicCheck: anEntity)
		ifTrue: [ aCriticBlock cull: (self critiqueFor: anEntity) ]