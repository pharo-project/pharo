services
textEditorIn: aThemedMorph text: aStringOrText title: aString entryText: defaultEntryText entryHeight: entryHeight
	"Answer the result of a text editor (multiline) dialog ( a string or nil if cancelled)
	with the given label and title."

	self questionSound play.
	^(aThemedMorph openModal: (
		(TextEditorDialogWindow newWithTheme: aThemedMorph theme)
			textFont: self textFont;
			title: aString;
			text: aStringOrText;
			entryText: defaultEntryText;
			entryHeight: entryHeight)) entryText