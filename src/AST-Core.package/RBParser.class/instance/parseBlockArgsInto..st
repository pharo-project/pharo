private-parsing
parseBlockArgsInto: node 
	| verticalBar args colons |
	args := OrderedCollection new: 2.
	colons := OrderedCollection new: 2.
	verticalBar := false.
	[currentToken isSpecial and: [currentToken value = $:]] whileTrue: 
			[colons add: currentToken start.
			self step.	":"
			verticalBar := true.
			args add: self parseVariableNode].
	verticalBar 
		ifTrue: 
			[currentToken isBinary 
				ifTrue: 
					[node bar: currentToken start.
					currentToken value = #| 
						ifTrue: [self step]
						ifFalse: 
							[currentToken value = #'||' 
								ifTrue: 
									["Hack the current token to be the start 
									of temps bar"

									currentToken
										value: #|;
										start: currentToken start + 1]
								ifFalse: [ args add:(self parserError: '''|'' expected')]]]
				ifFalse: 
					[(currentToken isSpecial and: [currentToken value = $]]) 
						ifFalse: [ args add:(self parserError: '''|'' expected')]]].
	node
		arguments: args;
		colons: colons.
	^node