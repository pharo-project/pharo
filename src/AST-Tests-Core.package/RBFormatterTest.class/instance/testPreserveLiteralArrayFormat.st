testing
testPreserveLiteralArrayFormat
	| inputSource literalArrayNode |
	"symbols within a literal array can omit the # character, if it is used that way,
	the formatter should not add a # character but just use the source form."
	inputSource := '#(#withnumbersign nonumbersign ''string'')'.
   literalArrayNode := RBParser parseExpression: inputSource.
	self assert: literalArrayNode source equals: literalArrayNode formattedCode.
