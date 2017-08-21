reading
readFrom: aReader
	| numberOfLiterals |
	name := aReader readByteSymbol.
	className := aReader readByteSymbol.
	protocol := aReader readByteSymbol.
	header := aReader readInt.
	bytecode := aReader readByteArray.

	numberOfLiterals := aReader readInt.
	literals := Array new: numberOfLiterals.
	1 to: numberOfLiterals do: [ :idx | literals at:idx put: (HEExportedLiteral readFrom: aReader) ]