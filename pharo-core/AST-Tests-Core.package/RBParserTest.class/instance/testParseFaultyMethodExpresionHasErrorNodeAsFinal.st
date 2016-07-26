tests parsing
testParseFaultyMethodExpresionHasErrorNodeAsFinal
	| node strangeExpression |
	
	strangeExpression := 'method: asd  ^ asd. n'.
	node := self parseFaultyMethod: strangeExpression.

	self assert: node isMethod.
	self assert: node isFaulty.
	self assert: node statements size equals: 2.
	self assert: node statements last  isFaulty
	