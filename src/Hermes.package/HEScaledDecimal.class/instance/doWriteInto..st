writing
doWriteInto: aWriter
	super doWriteInto: aWriter.
	value scale asExportedLiteral writeInto: aWriter.	