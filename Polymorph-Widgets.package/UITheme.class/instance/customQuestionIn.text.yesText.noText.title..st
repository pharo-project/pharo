services
customQuestionIn: aThemedMorph text: labelText yesText: yesText noText: noText title: aString
	"Answer the result of a question dialog with the given label, button labels and title."

	self questionSound play.
	^(aThemedMorph openModal: (
		(CustomQuestionDialogWindow newWithTheme: aThemedMorph theme)
			textFont: self textFont;
			title: aString;
			text: labelText;
			yesText: yesText help: nil;
			noText: noText help: nil)) answer