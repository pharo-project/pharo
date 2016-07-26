morphic
newTextMorph
	textMorph := self
		newTextEntryFor: self
		getText: #text
		setText: #text:
		help: nil.
	textMorph
		autoAccept: true;
		selectAll.
	textMorph textMorph
		on: #keyStroke send: #editorKeyStroke: to: self.
	^ textMorph