tests
testPrintOn
	"
	self debug: #testPrintOn
	"
	self assert: element printString =  'a SHStyleElement tokens: #(#self #super #true #false)'

	