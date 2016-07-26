pool variables
sharedPoolOfVarNamed: aString
	"Returns the SharedPool or nil from which the pool variable named aString is coming from."
	
	 ^ self sharedPools 
			detect: [:each | each usesClassVarNamed: aString ]
			ifNone: [
						self superclass == nil
							ifTrue: [nil]
							ifFalse: [self superclass sharedPoolOfVarNamed: aString]
						]