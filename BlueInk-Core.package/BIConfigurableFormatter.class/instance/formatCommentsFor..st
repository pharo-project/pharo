private-formatting
formatCommentsFor: aNode
	originalSource isNil
		ifTrue: [ ^ self ].
	aNode comments
		do:
			[ :each | 
			self basicFormatCommentFor: each.
			self newLine ]