accessing
initializeParserWith: aString 
	source := aString.
	self scanner: (self scannerClass on: (ReadStream on: aString) errorBlock: self errorBlock)		