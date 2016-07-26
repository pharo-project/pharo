as yet unclassified
newTextMorph
	"Answer a new text editor morph."

	|tm|
	tm := (self
		newTextEditorFor: self
		getText: #entryText
		setText: #entryText:
		getEnabled: nil)
			styled: false;
			minWidth: Display width // 4;
			minHeight: Display height // 4;
			disable.
	^tm