accessing
parseTree
	^parseTree isNil
		ifTrue: [parseTree := self buildParseTree]
		ifFalse: [parseTree]