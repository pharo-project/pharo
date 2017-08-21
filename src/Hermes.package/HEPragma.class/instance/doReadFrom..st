reading
doReadFrom: aReader
	method := HEExportedLiteral readFrom: aReader.
	keyword := HEExportedLiteral readFrom: aReader.
	arguments := HEExportedLiteral readFrom: aReader.