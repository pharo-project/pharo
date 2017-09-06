running
check: aMethod forCritiquesDo: aCriticBlock
	| a |
	a asString.
	a := 1.
	((RBReadBeforeWrittenTester variablesReadBeforeWrittenIn: aMethod ast) notEmpty) ifTrue: [ 
		aCriticBlock cull: (self critiqueFor: aMethod) ]