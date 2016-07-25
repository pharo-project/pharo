handling
handleIfNilIfNotNil: msgNode
	(self isIfNil: msgNode)
		ifTrue: [ ^ self convertToIfNil: msgNode ].
	(self isIfNotNil: msgNode)
		ifTrue: [ ^ self convertToIfNotNil: msgNode ]