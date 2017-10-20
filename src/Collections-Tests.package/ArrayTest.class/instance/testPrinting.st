tests
testPrinting
	"self debug: #testPrinting"
	
	self assert: literalArray printString = '#(1 true 3 #four)'.
	self assert: (literalArray = (self class compiler evaluate: literalArray printString)).
	self assert: (selfEvaluatingArray = (self class compiler evaluate: selfEvaluatingArray printString)).
	self assert: nonSEArray1 printString =  'an Array(1 a Set(1))'.
	self assert: nonSEarray2 printString = '{#Array->Array}'
