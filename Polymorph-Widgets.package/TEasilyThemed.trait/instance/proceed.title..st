services
proceed: aStringOrText title: aString
	"Open a proceed dialog and answer true if not cancelled, false otherwise."

	^self theme
		proceedIn: self
		text: aStringOrText
		title: aString