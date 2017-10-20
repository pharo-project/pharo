tests
testIsLiteral
	"We work with a copy of literalArray, to avoid corrupting the code."
	
	| aLiteralArray |
	aLiteralArray := literalArray copy.
	self assert: aLiteralArray isLiteral.
	aLiteralArray at: 1 put: self class.
	self deny: aLiteralArray isLiteral.
	self deny: (literalArray as: WeakArray) isLiteral description: 'instances of Array subclasses are not literal'.