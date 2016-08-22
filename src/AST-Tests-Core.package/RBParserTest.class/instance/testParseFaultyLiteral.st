tests parsing
testParseFaultyLiteral
	| faultyLiteral faultyLiteralArray node |
	"a literal or literal array object with an unknown character can be parse
	as faulty expression. If so, the error token (unknown character) needs to be consumed
	otherwise we get an endless loop"
	faultyLiteral := '¿'.
	faultyLiteralArray  := '#(¿)'.
	node := self parseFaultyExpression: faultyLiteral.
	self assert: node isFaulty.
	self assert: node isSequence.
	self assert: node statements first errorMessage equals:'Unknown character'.	
	node := self parseFaultyExpression: faultyLiteralArray.
	self assert: node isFaulty.
	self assert: node isLiteralArray.	
	self assert: node contents first errorMessage equals:'Unknown character'.