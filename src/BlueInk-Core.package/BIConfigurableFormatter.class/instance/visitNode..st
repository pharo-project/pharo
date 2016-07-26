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
		around: [ 
			needsParenthesis
				ifTrue: [ codeStream nextPutAll: self stringInsideParentheses ].
			super visitNode: aNode.
			(self formatCommentWithStatements or: [ aNode isMethod or: [ aNode isSequence ] ])
				ifFalse: [ self formatCommentsFor: aNode ].
			needsParenthesis
				ifTrue: [ codeStream nextPutAll: self stringInsideParentheses ] ]