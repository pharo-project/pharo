*UnifiedFFI
loadSymbol: moduleSymbol from: moduleNameOrClass 
	^ self 
		loadSymbol: moduleSymbol 
		module: moduleNameOrClass asFFILibrary moduleName.
	