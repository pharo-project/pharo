parsing
parseReturn
	| returnTypeTuple returnType |
	returnTypeTuple := self parseType.
	returnType := self requestor returnType: returnTypeTuple.
	returnType isExternalType
		ifTrue: [ returnType validateAfterParse: returnTypeTuple ].
	^ returnType