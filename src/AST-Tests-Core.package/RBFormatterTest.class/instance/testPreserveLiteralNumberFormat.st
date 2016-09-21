testing
testPreserveLiteralNumberFormat
	| inputSource numbersNode |
	"Literal numbers can be written in different ways, integer, integer with
	radix float, fractional constants. The formatter should not change the
	formatting."
	inputSource := '#(1 4r33 16r0F 0.02 2e-2 -1)'.
   numbersNode := RBParser parseExpression: inputSource.
	self assert: numbersNode source equals: numbersNode formattedCode.
