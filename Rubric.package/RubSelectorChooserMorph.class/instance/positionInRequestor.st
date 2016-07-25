ui-building
positionInRequestor
	|  caret  editor pos allowedArea |
	requestor ifNil: [^self].
	editor := requestor editor.
	caret := editor stopIndex.
	editor selectAt: caret.
	pos := editor selectionPosition: prefix.
	allowedArea := self allowedArea.
	(pos x + self width) > allowedArea width
		ifTrue: [pos := pos - (self width @ 0) - ((50 + 40)@ 0)].
	(pos y + self height) > allowedArea height
		ifTrue: [pos := (pos x @ (allowedArea height - self height - 5))].
	^ pos 
 