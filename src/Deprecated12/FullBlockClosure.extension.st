Extension { #name : 'FullBlockClosure' }

{ #category : '*Deprecated12' }
FullBlockClosure >> sender [
	"Answer the context that sent the message that created the receiver."
	<reflection: 'Message sending and code execution - Message send reification'>

	self deprecated:
		'The sender is the one of the Stack. BlockClosures do not have senders. The new CleanBlocks introduce blocks withput the outerContext. Now, clients should use the outerContext directly and deal with it being possibly nil.'.
	^ outerContext sender
]
