private-parsing
addParserError: errorMessage to: aNode
	| errorNode |
	errorNode := self parserError: errorMessage.
	aNode addFaultyNode: errorNode.
	^ aNode