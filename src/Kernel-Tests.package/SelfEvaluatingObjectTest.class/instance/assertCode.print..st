utilities
assertCode: code print: aString
	self assert: (self class evaluate: code) printString = aString