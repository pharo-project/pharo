*renraku
check: anEntity forCritiquesDo: aCriticBlock

	(thisContext sender findContextSuchThat: [ :c |
		c selector = #check:forCritiquesDo: ]) ifNotNil: [ 1 halt. ].

	(self basicCheck: anEntity)
		ifTrue: [ aCriticBlock cull: (self critiqueFor: anEntity) ]