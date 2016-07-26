error correction
declareGlobal: name
	| sym |
	sym := name asSymbol.
	^encoder
		global: (encoder environment
					at: sym put: nil;
					associationAt: sym)
		name: sym