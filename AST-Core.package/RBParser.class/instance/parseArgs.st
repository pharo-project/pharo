private-parsing
parseArgs
	| args |
	args := OrderedCollection new.
	[currentToken isIdentifier] whileTrue: [args add: self parseVariableNode].
	^args