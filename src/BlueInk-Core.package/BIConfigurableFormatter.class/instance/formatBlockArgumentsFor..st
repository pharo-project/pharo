private-formatting
formatBlockArgumentsFor: aBlockNode
	aBlockNode arguments isEmpty
		ifTrue: [ ^ self ].
	aBlockNode arguments
		do:
			[ :each | codeStream nextPut: $:.
			self visitNode: each.
			self formatCommentWithStatements
				ifTrue: [ self formatCommentsFor: each ].
			self space ].
	codeStream nextPutAll: '| '.
	(self willBeMultiline: aBlockNode body)
		ifTrue: [ self newLine ]