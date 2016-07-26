*Athens-Morphic
displayCaretAndSelectionOnAthensCanvas: anAthensCanvas
	selectionStart ifNil: [ ^ self ].
	selectionStart = selectionStop
		ifTrue: [ self drawCaretOnAthensCanvas: anAthensCanvas ]
		ifFalse: [ 
			| selection |
			caretRect := nil.
			selection := ParagraphSelectionBlock first: selectionStart last: selectionStop color: self selectionColor.
			self forLinesIn: anAthensCanvas clipRect do: [ :line | selection displayInLine: line onAthensCanvas: anAthensCanvas ] ]