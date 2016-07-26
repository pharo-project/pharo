services
textEntry: aStringOrText title: aString entryText: defaultEntryText
	"Open a text entry dialog."

	^self theme
		textEntryIn: self
		text: aStringOrText
		title: aString
		entryText: defaultEntryText