running
check: aMethod forCritiquesDo: aCriticBlock
	(aMethod messages anySatisfy: [ :each |
		(each isEmpty or: [ each first isUppercase or: [ literalNames includes: each ] ]) ]) ifTrue: [ 
		aCriticBlock cull: (self critiqueFor: aMethod) ]