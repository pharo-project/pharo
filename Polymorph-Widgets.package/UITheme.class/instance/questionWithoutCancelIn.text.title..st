services
questionWithoutCancelIn: aThemedMorph text: aStringOrText title: aString
	"Answer the result of a question dialog with the given label and title."

	self questionSound play.
	^(aThemedMorph openModal: (
		(QuestionWithoutCancelDialogWindow newWithTheme: aThemedMorph theme)
			textFont: self textFont;
			title: aString;
			text: aStringOrText)) answer