Extension { #name : 'FullBlockClosure' }

{ #category : '*Deprecated14' }
FullBlockClosure >> sender [
	"Answer the context that sent the message that created the receiver."
	self deprecated: 'Blocks do not have senders. As the outerContext or home context instead, but take care: clean blocks do not know the context!'.
	^outerContext sender
]
