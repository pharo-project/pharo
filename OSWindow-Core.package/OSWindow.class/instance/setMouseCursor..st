accessing
setMouseCursor: cursorWithMask
	currentCursor == cursorWithMask ifTrue: [ ^ self ].
	currentCursor := cursorWithMask.
	self validHandle setMouseCursor: cursorWithMask