parsing
parseExpression: aString onError: aBlock 
	| node parser |
	parser := self new.
	parser errorBlock: aBlock.
	parser initializeParserWith: aString.
	node := parser parseExpression: aString.
	^(node statements size == 1 and: [node temporaries isEmpty]) 
		ifTrue: [node statements first]
		ifFalse: [node]