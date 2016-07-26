services
longMessage: aStringOrText title: aString
	"Open a (long) message dialog."

	^self theme
		longMessageIn: self
		text: aStringOrText
		title: aString