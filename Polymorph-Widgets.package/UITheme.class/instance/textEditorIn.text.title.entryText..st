services
textEditorIn: aThemedMorph text: aStringOrText title: aString entryText: defaultEntryText
	"Answer the result of a text editor (multiline) dialog ( a string or nil if cancelled)
	with the given label and title."

	^self
		textEditorIn: aThemedMorph
		text: aStringOrText
		title: aString
		entryText: defaultEntryText
		entryHeight: 64