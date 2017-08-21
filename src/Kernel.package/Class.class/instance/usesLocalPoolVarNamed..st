pool variables
usesLocalPoolVarNamed: aString
	"Return whether the receiver uses a pool variable named: aString which is defined locally"
	
	self hasSharedPools
		ifTrue: [ self sharedPools do: [:each | (each usesClassVarNamed: aString) 
												ifTrue: [ ^true ]]]
		ifFalse: [ ^false ].
	^false