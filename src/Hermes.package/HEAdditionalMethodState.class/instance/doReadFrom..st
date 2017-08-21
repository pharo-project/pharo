reading
doReadFrom: aReader
	method := HEExportedLiteral readFrom: aReader.
	selector := HEExportedLiteral readFrom: aReader.
	value := HEExportedLiteral readFrom: aReader.
