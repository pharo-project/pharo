accessing
parseMethod: aString
	| node |
	node := self parseMethod.
	node source: aString.
	self atEnd
		ifFalse: [ 
			 ^ self parseIncompleteExpression: node ].
	node source: aString.
	^ node