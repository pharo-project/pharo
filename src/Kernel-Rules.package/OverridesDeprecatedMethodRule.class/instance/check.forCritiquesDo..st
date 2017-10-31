running
check: aMethod forCritiquesDo: aCriticBlock

	(aMethod overriddenMethods anySatisfy: #isDeprecated) ifTrue: [ 
		aCriticBlock cull: (self critiqueFor: aMethod) ]
