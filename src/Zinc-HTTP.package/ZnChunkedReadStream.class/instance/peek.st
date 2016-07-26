accessing
peek
	self ensureChunkOrAtEnd.
	self atEnd ifTrue: [ ^ nil ].
	^ chunk at: (position + 1)