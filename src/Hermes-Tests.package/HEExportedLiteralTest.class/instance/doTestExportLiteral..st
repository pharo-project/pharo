tests
doTestExportLiteral: original 
	| exported |
	original asExportedLiteral writeInto: writer.
	writer flush.
	
	exported := (HEExportedLiteral readFrom: reader) value.	
	self assert: exported equals: original.