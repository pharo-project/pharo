tests - lines
testLineCount
	| sampleCRString sampleLFString sampleCRLFString |
	sampleCRString := 'Fred', String cr , 'the' , String cr , 'Bear'.
	sampleLFString := 'Fred', String lf , 'the' , String lf , 'Bear'.
	sampleCRLFString := 'Fred', String crlf , 'the' , String crlf , 'Bear'.
	
	self assert: sampleCRString lineCount = 3.
	self assert: sampleLFString lineCount = 3.
	self assert: sampleCRLFString lineCount = 3.