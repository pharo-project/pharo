as yet unclassified
yesText: aStringOrText help: helpString
	"Set the yes button label.
	if the text is nil, remove the button."

	aStringOrText
		ifNil: [self cancelButton delete]
		ifNotNil: [self yesButton
					hResizing: #shrinkWrap;
					label: aStringOrText;
					setBalloonText: helpString]