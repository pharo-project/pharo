services
textEntryIn: aThemedMorph text: aStringOrText title: aString entryText: defaultEntryText entryCompletion: anEntryCompletion
	"Answer the result of a text entry dialog ( a string or nil if cancelled)
	with the given label and title."

	self questionSound play.
	^(aThemedMorph openModal: (
		(TextEntryDialogWindow newWithTheme: aThemedMorph theme)
			textFont: self textFont;
			title: aString;
			text: aStringOrText;
			entryText: defaultEntryText;
			entryCompletion: anEntryCompletion)) entryText