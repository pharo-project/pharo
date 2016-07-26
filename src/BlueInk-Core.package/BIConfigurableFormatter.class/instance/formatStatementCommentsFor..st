private-formatting
formatStatementCommentsFor: aStatementNode
	originalSource isNil
		ifTrue: [ ^ self ].
	self formatCommentWithStatements
		ifFalse: [ ^ self ].
	aStatementNode statementComments
		do:
			[ :each | 
			| count |
			count := self newLinesBeforeStartingAt: each start.
			self retainBlankLinesBeforeComments & (count > 0)
				ifTrue: [ self newLines: count ]
				ifFalse: [ codeStream tab ].
			self useBasicCommentFormat
				ifTrue: [ self basicFormatCommentFor: each ]
				ifFalse: [ 
			self resizeCommentFor: each startingAt: self currentLineLength ]]