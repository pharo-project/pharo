accessing
setMouseCursor: cursor mask: mask
	currentCursor == cursor ifTrue: [ ^ self ].
	currentCursor := cursor.

	self validHandle setMouseCursor: cursor mask: mask