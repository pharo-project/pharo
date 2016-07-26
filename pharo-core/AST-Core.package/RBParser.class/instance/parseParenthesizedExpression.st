private-parsing
parseParenthesizedExpression
	| leftParen node |
	leftParen := currentToken start.
	self step.
	node := self parseAssignment.
	^(currentToken isSpecial and: [currentToken value = $)])
		ifTrue: 
			[node addParenthesis: (leftParen to: currentToken start).
			self step.
			node]
		ifFalse: [ self parserError: ''')'' expected']