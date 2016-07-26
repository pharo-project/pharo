private - layout
updateBounds
	| x line |
	self textArea ifNil: [ ^self ].
	line := self textArea selectionStop textLine.
	x := self textArea selectionStop left.
	self position: x @ line top.
	self extent: self class defaultWidth @ (line height + 1)