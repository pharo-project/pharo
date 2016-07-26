error correction
declareClassVar: name
	| sym class |
	sym := name asSymbol.
	class := encoder classEncoding.
	class := class theNonMetaClass.		"not the metaclass"
	class addClassVarNamed: name.
	^ encoder global: (class classPool associationAt: sym)
			name: sym