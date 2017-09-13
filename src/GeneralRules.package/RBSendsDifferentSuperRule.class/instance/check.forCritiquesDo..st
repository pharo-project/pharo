running
check: aMethod forCritiquesDo: aCriticBlock

	(aMethod superMessages anySatisfy: [ :message |
		message ~= aMethod selector ]) ifTrue: [ 
		aCriticBlock cull: (self critiqueFor: aMethod) ]
	
