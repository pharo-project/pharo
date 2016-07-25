visiting
visitNode: aNode
	| needsParenthesis |
	(lookaheadCode includesKey: aNode)
		ifTrue: [ ^ self writeString: (lookaheadCode at: aNode) ].
	needsParenthesis := self needsParenthesisFor: aNode.
	self
		bracketWith:
			(needsParenthesis
				ifTrue: [ '()' ]
				ifFalse: [ '' ])
		around: [ super visitNode: aNode ]