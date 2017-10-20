writing
doWriteInto: aWriter
	value numerator asExportedLiteral writeInto: aWriter.
	value denominator asExportedLiteral writeInto: aWriter.	