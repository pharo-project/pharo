private
consumeComment
	stream peek = $/ ifTrue: [ self consumeToEOL ].
	stream peek = $* ifTrue: [ self consumeToCommentEnd ]