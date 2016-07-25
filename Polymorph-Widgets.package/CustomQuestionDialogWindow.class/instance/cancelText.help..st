as yet unclassified
cancelText: aStringOrText help: helpString
	"Set the cancel button label.
	if the text is nil, remove the button."

	aStringOrText
		ifNil: [self cancelButton delete]
		ifNotNil: [self cancelButton
					hResizing: #shrinkWrap;
					label: aStringOrText;
					setBalloonText: helpString]