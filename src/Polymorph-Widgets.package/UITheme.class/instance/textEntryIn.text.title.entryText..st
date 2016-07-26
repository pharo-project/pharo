services
textEntryIn: aThemedMorph text: aStringOrText title: aString entryText: defaultEntryText
	"Answer the result of a text entry dialog ( a string or nil if cancelled)
	with the given label and title."

	self questionSound play.
	^(aThemedMorph openModal: (
		TextEntryDialogWindow new
			textFont: self textFont;
			title: aString;
			text: aStringOrText;
			entryText: defaultEntryText)) entryText