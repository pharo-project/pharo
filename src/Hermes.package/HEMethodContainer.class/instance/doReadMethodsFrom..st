reading
doReadMethodsFrom: aReader
	| numberOfMethods numberOfClassMethods |
	category := aReader readByteSymbol.
	numberOfMethods := aReader readInt.

	methods := (1 to: numberOfMethods) collect: [ :i | HEMethod readFrom: aReader ] as: Array.
	numberOfClassMethods := aReader readInt.

	classSideMethods := (1 to: numberOfClassMethods) collect: [ :i | HEMethod readFrom: aReader ] as: Array.
	traitComposition := HEExportedLiteral readFrom: aReader.

	classTraitComposition := HEExportedLiteral readFrom: aReader