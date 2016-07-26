private-formatting
formatStatementCommentsFor: aStatementNode
	originalSource isNil
		ifTrue: [ ^ self ].
	aStatementNode statementComments
		do:
			[ :each | 
			 self newLine. 
			 self basicFormatCommentFor: each ]