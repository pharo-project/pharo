accessing
specialPrimitiveSimulators
	SpecialPrimitiveSimulators ifNil: [ self initializePrimitiveSimulators ].
	^ SpecialPrimitiveSimulators