initialize
isPrimitiveNativeCall: aCompiledMethod
	aCompiledMethod isPrimitive
		ifFalse: [ ^ false ].
	^ (aCompiledMethod pragmas anySatisfy: [ :aPragma | aPragma arguments isNotEmpty and: [ (aPragma argumentAt: 1) = 'primitiveNativeCall' ] ])