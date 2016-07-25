services
chooseColorIn: aThemedMorph title: aString color: aColor
	"Answer the result of a color selector dialog with the given title and initial color."

	|d|
	d := aThemedMorph openModal: (
		(ColorSelectorDialogWindow newWithTheme: aThemedMorph theme)
			title: aString;
			selectedColor: aColor).
	^d  cancelled
		ifFalse: [d selectedColor]