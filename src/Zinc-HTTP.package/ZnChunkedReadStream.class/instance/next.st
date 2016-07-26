accessing
next
	self ensureChunkOrAtEnd.
	self atEnd ifTrue: [ ^ nil ].
	^ chunk at: (position := position + 1)