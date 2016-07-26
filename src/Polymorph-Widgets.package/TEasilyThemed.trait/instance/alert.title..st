services
alert: aStringOrText title: aString
	"Open an alert dialog."

	^self
		alert: aStringOrText
		title: aString
		configure: [:d | ]