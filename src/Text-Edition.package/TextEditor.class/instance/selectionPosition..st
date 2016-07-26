accessing
selectionPosition: aString

	| startIndex |
	startIndex := self startIndex.
	^[
		| bottomLeft topLeft index |
		index := startIndex - aString size.
		self selectInvisiblyFrom: index to: index - 1.
		bottomLeft := self paragraph selectionRects first bottomLeft.
		topLeft := self morph owner submorphBounds topLeft.
		topLeft + bottomLeft ]
			ensure: [ self selectInvisiblyFrom: startIndex to: startIndex - 1 ]