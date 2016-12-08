stubs
initializeClassPool: aClassName

	| definition class classPool classVariableBindings |
	definition := systemDefinition classNamed: aClassName.
	classVariableBindings := definition classVarNames collect: [ :each | | classVar |
		classVar := (self classNamed: #ClassVariable) basicNew.
		classVar instanceVariableAtIndex: 1 put: (each asLiteralInObjectSpace: builder objectSpace) ].
	
	class := environment at: aClassName.
	classPool := builder bootstrapInterpreter
		evaluateCode: 'aClassVariableNamesCollection asDictionary'
		withTemps: { 
			'aClassVariableNamesCollection' -> (builder objectSpace newArrayWithAll: classVariableBindings)
		}.
	class className: aClassName.
	class classPool: classPool.
	^ class