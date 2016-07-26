accessing
selectionPosition: aString

	| startIndex |
	startIndex := self startIndex.
	^[
		| bottomLeft topLeft index |
		index := startIndex - aString size.
		self selectInvisiblyFrom: index to: index - 1.
		bottomLeft := textArea cursor bottomLeft.
		topLeft := textArea owner positionInWorld.
		topLeft + bottomLeft - textArea offset]
			ensure: [ self selectInvisiblyFrom: startIndex to: startIndex - 1 ]