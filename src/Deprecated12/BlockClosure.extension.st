Extension { #name : 'BlockClosure' }

{ #category : '*Deprecated12' }
BlockClosure >> sender [
	<reflection: 'Message sending and code execution - Message send reification'>

	self deprecated:
		'The sender is the one of the Stack. BlockClosures do not have senders. The new CleanBlocks introduce blocks withput the outerContext. Now, clients should use the outerContext directly and deal with it being possibly nil.'.
	^ self subclassResponsibility
]
