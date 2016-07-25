visiting
visitMethod: anIr
	IRFix new visitNode: anIr.
	self pushScope: anIr.
	gen irPrimitive: anIr irPrimitive.
	gen numArgs: anIr numArgs.
	gen properties: anIr properties.
	gen numTemps: (anIr tempMap size).
	gen additionalLiterals: anIr additionalLiterals.
	gen forceLongForm: anIr forceLongForm.
	self visitSequences: anIr allSequences.