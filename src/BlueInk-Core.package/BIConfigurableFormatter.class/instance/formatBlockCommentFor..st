private-formatting
formatBlockCommentFor: aBlockNode
	originalSource isNil
		ifTrue: [  ^ self ].
	aBlockNode comments
		do: [ :each | 
			
			self basicFormatCommentFor: each.
			(self isNonEmptySingleLineBlock: aBlockNode)
				ifTrue: [  self space ].
			(self willBeMultiline: aBlockNode body)
				ifTrue: [  self newLine ] ]