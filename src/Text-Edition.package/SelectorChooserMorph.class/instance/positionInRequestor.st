ui-building
positionInRequestor
	| caret bottomLeft topLeft editor pos allowedArea |
	requestor ifNil: [^self].
	editor := requestor editor.
	caret := editor stopIndex.
	[editor selectAt: caret.
	bottomLeft := editor paragraph selectionRects first bottomLeft.
	topLeft := requestor owner submorphBounds topLeft.
	pos := topLeft + bottomLeft]
		ensure: [editor selectAt: caret].
	pos := pos + (40 @ 0).
	allowedArea := self allowedArea.
	(pos x + self width) > allowedArea width
		ifTrue: [pos := pos - (self width @ 0) - ((50 + 40)@ 0)].
	(pos y + self height) > allowedArea height
		ifTrue: [pos := (pos x @ (allowedArea height - self height - 5))].
	^ pos.
