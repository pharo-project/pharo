reading
readFrom: aReader
	| numberOfLiterals |

	name := aReader readByteSymbol.
	className := aReader readByteSymbol.
	protocol := aReader readByteSymbol.
		
	encoderClassName := aReader readByteString.
	(encoderClassName beginsWith: 'Opal') ifTrue: [ 
		encoderClassName := encoderClassName withoutPrefix: 'Opal' ].
	numArgs := aReader readUInt32.
	numTemps := aReader readUInt32.
	hasPrimitive := aReader readUInt32.

	bytecode := aReader readByteArray.

	numberOfLiterals := aReader readInt32.
	literals := Array new: numberOfLiterals.
	1 to: numberOfLiterals do: [ :idx | literals at:idx put: (HEExportedLiteral readFrom: aReader) ]