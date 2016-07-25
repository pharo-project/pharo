services
alert: aStringOrText title: aString configure: aBlock
	"Open an alert dialog.
	Configure the dialog with the 1 argument block
	before opening modally."

	^self theme
		alertIn: self
		text: aStringOrText
		title: aString
		configure: aBlock