private-formatting
formatBlockArgumentsFor: aBlockNode
	aBlockNode arguments isEmpty
		ifTrue: [ ^ self ].
	aBlockNode arguments
		do:
			[ :each | 
			codeStream nextPut: $:.
			self visitNode: each.
			self formatCommentsFor: each.
			self space ].
	codeStream nextPutAll: '| '