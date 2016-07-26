updating
updateSourceCodeAreaWith: aText
	self sourceTextModel setText: aText.
	self sourceTextModel hasUnacceptedEdits: false.
	