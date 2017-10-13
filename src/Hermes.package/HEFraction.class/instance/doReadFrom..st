reading
doReadFrom: aReader
	| denominator numerator |
	numerator := HEExportedLiteral readFrom: aReader.
	denominator := HEExportedLiteral readFrom: aReader.
	
	value := Fraction numerator: numerator value denominator: denominator value.