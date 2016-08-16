private-parsing
parseStatements: pragmaBoolean into: aSequenceNode
	| temps leftBar rightBar |
	temps := #().
	leftBar := rightBar := nil.
	currentToken isBinary 
		ifTrue: 
			[currentToken value = #| 
				ifTrue: 
					[leftBar := currentToken start.
					self step.
					temps := self parseTemps.
					(currentToken isBinary and: [currentToken value = #|]) 
						ifFalse: [ self addParserError: '''|'' expected' to: aSequenceNode.
							^ aSequenceNode].
					rightBar := currentToken start.
					self step]
				ifFalse: 
					[currentToken value = #'||' 
						ifTrue: 
							[rightBar := (leftBar := currentToken start) + 1.
							self step]]].
	^self parseStatementList: pragmaBoolean
		into: (aSequenceNode 
				leftBar: leftBar
				temporaries: temps
				rightBar: rightBar)