services
proceedIn: aThemedMorph text: aStringOrText title: aString
	"Answer the result of a proceed dialog with the given label and title."

	self questionSound play.
	^(aThemedMorph openModal: (
		(ProceedDialogWindow newWithTheme: aThemedMorph theme)
			textFont: self textFont;
			title: aString;
			text: aStringOrText)) cancelled not