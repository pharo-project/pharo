expression types
blockExpression
	"[ ({:var} |) (| {temps} |) (statements) ] => BlockNode."

	| blockNode variableNodes temporaryBlockVariables start |
	blockNode := BlockNode new.
	variableNodes := OrderedCollection new.
	start := prevMark + requestorOffset.
	"Gather parameters."
	[self match: #colon] whileTrue:
		[variableNodes addLast: (encoder bindBlockArg: self argumentName within: blockNode)].
	(variableNodes size > 0 & (hereType ~~ #rightBracket) and: [(self match: #verticalBar) not]) ifTrue:
		[^self expected: 'Vertical bar'].

	temporaryBlockVariables := self temporaryBlockVariablesFor: blockNode.
	self statements: variableNodes innerBlock: true blockNode: blockNode.
	blockNode temporaries: temporaryBlockVariables.

	(self match: #rightBracket) ifFalse: [^self expected: 'Period or right bracket'].

	blockNode noteSourceRangeStart: start end: self endOfLastToken encoder: encoder.

	"The scope of the parameters and temporary block variables is no longer active."
	temporaryBlockVariables do: [:variable | variable scope: -1].
	variableNodes do: [:variable | variable scope: -1]