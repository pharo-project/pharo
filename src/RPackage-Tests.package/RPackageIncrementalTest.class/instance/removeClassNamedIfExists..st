utility
removeClassNamedIfExists: aClassNameSymbol
	Smalltalk at: aClassNameSymbol asSymbol ifPresent: [:c| c removeFromSystem]