enumerating
check: node forCritiquesDo: aCriticBlock
	node isMessage ifFalse: [ ^ self ].
	node receiver isBlock ifFalse: [ ^ self ].
	node parent isCascade ifTrue: [ ^ self ].
	(self blockEvaluatingMessages includes: node selector)
		ifFalse: [ ^ self ].
	
	aCriticBlock cull: (self critiqueFor: node)
