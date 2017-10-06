tests
export: original
	original asExportedLiteral writeInto: writer.
	writer flush.
	
	^ (HEExportedLiteral readFrom: reader) value.	
