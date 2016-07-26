private-parsing
parseKeywordPattern
	| selector positions args node |
	selector := ''.
	positions := OrderedCollection new: 2.
	args := OrderedCollection new: 2.
	[currentToken isKeyword]
		whileTrue:
			[selector := selector, currentToken value.
			 positions add: currentToken start.
			self step.
			args add: self parseVariableNode].
	node := self methodNodeClass
		selector: selector asSymbol
		keywordsPositions: positions asIntegerArray
		arguments: args.
	node comments: (node comments, args last comments).
	args last comments ifNotEmpty: [ args last comments: #() ].
	^node