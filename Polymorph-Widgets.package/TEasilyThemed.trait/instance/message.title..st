services
message: aStringOrText title: aString
	"Open a message dialog."

	^self theme
		messageIn: self
		text: aStringOrText
		title: aString