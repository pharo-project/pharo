reading
doReadFrom: aReader
	super doReadFrom: aReader.
	subject := HEExportedLiteral readFrom: aReader.