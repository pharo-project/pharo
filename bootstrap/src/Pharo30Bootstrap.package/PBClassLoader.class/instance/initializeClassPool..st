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
	
	"Force the creation of the symbol using the object space class table." 
	self flag: #bug. "If we do not use basicSetClassName, we duplicate symbols"
	class basicSetClassName: (aClassName asLiteralInObjectSpace: builder objectSpace).
	class classPool: classPool.
	^ class