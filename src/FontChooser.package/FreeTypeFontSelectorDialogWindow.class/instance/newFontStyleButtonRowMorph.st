instance creation
newFontStyleButtonRowMorph
	"Answer a new font style button row morph."

	^self newRow: {
		self newBoldButtonMorph.
		self newItalicButtonMorph}