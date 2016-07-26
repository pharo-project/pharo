services
customQuestionIn: aThemedMorph text: labelText yesText: yesText noText: noText cancelText: cancelText default: defaultOption title: aString
	"Answer the result of a question dialog with the given label, button labels and title.
	defaultOption should be one of true, false or nil to set the default button."

	self questionSound play.
	^(aThemedMorph openModal: (
		(CustomQuestionDialogWindow newWithTheme: aThemedMorph theme)
			textFont: self textFont;
			title: aString;
			text: labelText;
			yesText: yesText help: nil;
			noText: noText help: nil;
			cancelText: cancelText help: nil;
			default: defaultOption)) answer