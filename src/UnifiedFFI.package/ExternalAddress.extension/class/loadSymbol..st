*UnifiedFFI
loadSymbol: moduleSymbol
	"Load global symbol (one that you can find in current VM binary)"
	^ self 
		loadSymbol: moduleSymbol 
		module: nil