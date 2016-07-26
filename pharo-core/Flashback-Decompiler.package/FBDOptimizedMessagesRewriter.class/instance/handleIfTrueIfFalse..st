handling
handleIfTrueIfFalse: msgNode
	(self areArgumentsBlocks: msgNode) ifFalse: [ ^ self ].
	(self isIfFalse: msgNode) ifTrue: [ ^ self convertToIfFalse: msgNode ].
	(self isIfTrue: msgNode) ifTrue: [ ^ self convertToIfTrue: msgNode ].
 	(self isAnd: msgNode) ifTrue: [ ^self convertToAnd: msgNode ].
	(self isOr: msgNode) ifTrue: [ ^ self convertToOr: msgNode ].
	(self isIfNilIfNotNil: msgNode) ifTrue: [ ^ self convertToIfNilIfNotNil: msgNode ]