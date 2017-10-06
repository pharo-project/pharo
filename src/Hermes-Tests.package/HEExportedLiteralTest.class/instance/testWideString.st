tests
testWideString
	self doTestExportLiteral: '1796exportFixes-tkMX' asWideString.
	self doTestExportLiteral: (Character value: 257) asString.
