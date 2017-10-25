tests - lines
testLineNumber
	| sampleCRString sampleLFString sampleCRLFString |
	sampleCRString := 'Fred', String cr , 'the' , String cr , 'Bear'.
	sampleLFString := 'Fred', String lf , 'the' , String lf , 'Bear'.
	sampleCRLFString := 'Fred', String crlf , 'the' , String crlf , 'Bear'.
	
	self assert: (sampleCRString lineNumber: 2) = 'the'.
	self assert: (sampleLFString lineNumber: 2) = 'the'.
	self assert: (sampleCRLFString lineNumber: 2) = 'the'.