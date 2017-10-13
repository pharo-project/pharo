reading
doReadFrom: aReader
	| scale |
	super doReadFrom: aReader.
	scale := HEExportedLiteral readFrom: aReader.
	
	value := ScaledDecimal newFromNumber: value scale: scale value.
	
	