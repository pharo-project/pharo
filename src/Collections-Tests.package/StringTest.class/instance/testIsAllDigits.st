tests
testIsAllDigits
	"self run: #testIsAllDigits"
	self assert: '' isAllDigits = false.
	self assert: '1.23' isAllDigits = false.
	self assert: '12 3' isAllDigits = false.
	self assert: '-123' isAllDigits = false.
	self assert: '0123456789' isAllDigits = true.