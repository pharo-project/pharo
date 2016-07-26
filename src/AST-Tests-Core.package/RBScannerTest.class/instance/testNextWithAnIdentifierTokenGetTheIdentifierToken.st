next
testNextWithAnIdentifierTokenGetTheIdentifierToken
	| source scanner token |
	
	source := 'identifierToken'.
	scanner := self buildScannerForText: source.
	token := scanner next.
	self assert: token isIdentifier.
	self deny: token isError.
	self assert: token value equals: source.