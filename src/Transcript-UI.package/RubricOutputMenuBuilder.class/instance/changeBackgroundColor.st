coloring
changeBackgroundColor

	(UIManager default chooseColor: output textMorph color)
		ifNotNil: [:newColor | output textMorph color: newColor].