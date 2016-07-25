tests
testPrintString

	| locale |
	locale := LocaleID isoString: 'en-us'.	
	self assert: locale printString = 'en-us'
