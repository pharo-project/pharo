running
check: aMethod forCritiquesDo: aCriticBlock
	(aMethod protocol = Protocol unclassified) ifTrue: [ 
		aCriticBlock cull: (self critiqueFor: aMethod) ]
		