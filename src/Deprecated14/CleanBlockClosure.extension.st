Extension { #name : 'CleanBlockClosure' }

{ #category : '*Deprecated14' }
CleanBlockClosure >> sender [
	" clean blocks do not know the sender"
	self deprecated: 'Blocks do not have senders. As the outerContext or home context instead, but take care: clean blocks do not know the context!'.
	^nil
]
