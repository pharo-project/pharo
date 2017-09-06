running
check: aMethod forCritiquesDo: aCriticBlock

	aMethod ast statements size >= self longMethodSize
		ifTrue: [ aCriticBlock cull: (self critiqueFor: aMethod) ]