visitor-double dispatching
visitMethodNode: aMethodNode
	
	aMethodNode arguments size > 15 ifTrue: [self backendError: 'Too many arguments' forNode: aMethodNode ].

	methodBuilder compilationContext: aMethodNode compilationContext.
	methodBuilder addTemps: aMethodNode scope tempVarNames.
		
	methodBuilder properties: aMethodNode properties.
	methodBuilder irPrimitive: aMethodNode primitiveFromPragma.
	aMethodNode pragmas do: [:each | self visitPragmaNode: each].
	methodBuilder numArgs: aMethodNode arguments size.
	
	aMethodNode scope tempVector ifNotEmpty: [
		methodBuilder 
			createTempVectorNamed: aMethodNode scope tempVectorName 
			withVars: (aMethodNode scope tempVector collect: [:each| each name]) asArray.
	].
	effectTranslator visitNode: aMethodNode body.
	aMethodNode body lastIsReturn ifFalse:  [methodBuilder pushReceiver; returnTop]