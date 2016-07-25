registering simulated primitives
simulatePrimitive: primName module: moduleName with: simulator
	^ self specialPrimitiveSimulators at: {primName. moduleName} put: simulator