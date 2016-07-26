tests
testPrintStringHex
	self assert: $a printStringHex = '61'.
	self assert: Character space printStringHex = '20'