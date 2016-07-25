services
abort: aStringOrText title: aString
	"Open an error dialog."

	^self theme
		abortIn: self
		text: aStringOrText
		title: aString