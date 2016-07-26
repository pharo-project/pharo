analyzing
referencesOf: aCompiledMethod do: aBlock

	aCompiledMethod fuelPrepare.
	1 to: aCompiledMethod numLiterals
		do: [ :index | aBlock value: (aCompiledMethod literalAt: index) ]