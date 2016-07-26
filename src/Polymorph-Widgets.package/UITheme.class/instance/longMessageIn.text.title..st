services
longMessageIn: aThemedMorph text: aStringOrText title: aString
	"Answer the result of a (potentially long) message dialog (true) with the given label and title."

	self messageSound play.
	^(aThemedMorph openModal: (
		(LongMessageDialogWindow newWithTheme: aThemedMorph theme)
			textFont: self textFont;
			title: aString;
			text: aStringOrText)) cancelled not