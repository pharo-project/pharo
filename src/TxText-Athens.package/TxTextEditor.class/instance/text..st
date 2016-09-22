initialization
text: aText

	text := aText.
	selection := text endPosition asSelection.
	self
		resetUndoBuffer;
		notifyNewText;
		notifySelectionChange: selection