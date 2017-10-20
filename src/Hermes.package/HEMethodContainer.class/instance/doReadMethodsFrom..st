reading
doReadMethodsFrom: aReader
	| numberOfMethods numberOfClassMethods |
	category := aReader readByteSymbol.
	numberOfMethods := aReader readInt32.

	methods := (1 to: numberOfMethods) collect: [ :i | HEMethod readFrom: aReader ] as: Array.
	numberOfClassMethods := aReader readInt32.

	classSideMethods := (1 to: numberOfClassMethods) collect: [ :i | HEMethod readFrom: aReader ] as: Array.
	traitComposition := HEExportedLiteral readFrom: aReader.

	classTraitComposition := HEExportedLiteral readFrom: aReader