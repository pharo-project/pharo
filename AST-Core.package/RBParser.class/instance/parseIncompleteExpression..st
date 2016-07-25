private-parsing
parseIncompleteExpression: priorStatementsNode
	"some incomplete expressions followed. Add a parserError node
 to the prior statements, but not if priopriorStatementsNode  is already 
 an errorNode"
	 ^ priorStatementsNode isFaulty
		ifTrue: [ priorStatementsNode ]
		ifFalse:
			[ | errorNode |
			errorNode := self parserError: 'Unknown input at end'.
			errorNode ifNotNil: [ priorStatementsNode statements: priorStatementsNode statements , {errorNode} ].
			priorStatementsNode ]