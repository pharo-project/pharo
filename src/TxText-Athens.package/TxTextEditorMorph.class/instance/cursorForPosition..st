event handling
cursorForPosition: aPoint
	| pos |
	pos := layoutViewMorph textPositionAt: (aPoint - layoutViewMorph innerBounds origin).
	^ pos span attributeAt: TxMouseCursorAttribute.
	