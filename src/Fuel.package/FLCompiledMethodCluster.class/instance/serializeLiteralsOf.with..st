protected
serializeLiteralsOf: aCompiledMethod with: anEncoder

	aCompiledMethod fuelPrepare.
	1 to: aCompiledMethod numLiterals 
		do: [ :index | anEncoder encodeReferenceTo: (aCompiledMethod literalAt: index) ]