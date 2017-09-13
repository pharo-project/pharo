running
check: aMethod forCritiquesDo: aCriticBlock
	(aMethod timeStamp isEmpty) ifTrue: [ 
		aCriticBlock cull: (self critiqueFor: aMethod) ]