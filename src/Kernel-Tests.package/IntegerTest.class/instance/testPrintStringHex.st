tests - printing
testPrintStringHex
	"self run: #testPrintStringHex"
	self assert: 0 printStringHex = '0'.
	self assert: 12 printStringHex = 'C'.
	self assert: 1234 printStringHex = '4D2'.