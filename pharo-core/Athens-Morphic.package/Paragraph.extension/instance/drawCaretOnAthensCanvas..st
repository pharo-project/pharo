*Athens-Morphic
drawCaretOnAthensCanvas: anAthensCanvas
	(self showCaret not or: [ selectionStart isNil ])
		ifTrue: [ ^ self ].
	caretRect := selectionStart topLeft asIntegerPoint extent: 1 @ selectionStart height.
	anAthensCanvas setPaint: self insertionPointColor.
	anAthensCanvas drawShape: self caretRect