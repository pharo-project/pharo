utility
removeClassNamedIfExists: aClassNameSymbol

	Smalltalk globals at: aClassNameSymbol asSymbol ifPresent: [ :c | c removeFromSystem ]