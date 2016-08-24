private-parsing
parseStatementList: pragmaBoolean into: sequenceNode 
	| statements return periods returnPosition node |
	return := false.
	statements := OrderedCollection new.
	periods := OrderedCollection new.
	self addCommentsTo: sequenceNode.
	pragmaBoolean ifTrue: [self parsePragmas].
	[currentToken isSpecial and: [currentToken value = $.]] whileTrue: 
		[periods add: currentToken start.
		self step].
	[self atEnd 
		or: [currentToken isSpecial and: ['])}' includes: currentToken value]]] 
			whileFalse: [
				return ifTrue: [ 
					(self parserError: 'End of statement list encountered') 
						ifNotNil: [ :errorNode | 
								statements add: errorNode. 
								sequenceNode statements: statements.
								sequenceNode periods: periods.
								self step.
								^sequenceNode]
					].
				(currentToken isSpecial and: [currentToken value = $^]) 
					ifTrue: 
						[returnPosition := currentToken start.
						self step.
						node := self returnNodeClass return: returnPosition
									value: self parseAssignment.
						statements add: node.
						return := true]
					ifFalse: [node := self parseAssignment.
								statements add: node ].
				(currentToken isSpecial and: [currentToken value = $.]) 
					ifTrue: 
						[periods add: currentToken start.
						self step.
						self addCommentsTo: node]
					ifFalse: [return := true].
				[currentToken isSpecial and: [currentToken value = $.]] whileTrue: 
					[periods add: currentToken start.
					self step]
				].
	statements notEmpty ifTrue: [self addCommentsTo: statements last].
	sequenceNode
		statements: statements;
		periods: periods.
	^sequenceNode