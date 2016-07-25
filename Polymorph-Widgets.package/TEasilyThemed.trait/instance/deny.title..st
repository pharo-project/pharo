services
deny: aStringOrText title: aString
	"Open a denial dialog."

	^self theme
		denyIn: self
		text: aStringOrText
		title: aString