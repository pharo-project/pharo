tests
testCharacterSetBinaryNumber
	"self debug: #testCharacterSetBinaryNumber"
	
	"Using plus operator, we can build the following binary number
recognizer:"
	self assert: ('10010100' matchesRegex: '[01]+').	 	
	self deny: ('10001210' matchesRegex: '[01]+')	 