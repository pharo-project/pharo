running
check: aMethod forCritiquesDo: aCriticBlock
	((aMethod refersToLiteral: trueBinding) or: [ aMethod refersToLiteral: falseBinding ]) ifTrue: [ 
		aCriticBlock cull: (self critiqueFor: aMethod) ]