services
centeredAlert: aStringOrText title: aString configure: aBlock
	"Open an alert dialog.
	Configure the dialog with the 1 argument block
	before opening modally."

	^self theme
		centeredAlertIn: self
		text: aStringOrText
		title: aString
		configure: aBlock