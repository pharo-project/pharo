as yet unclassified
noText: aStringOrText help: helpString
	"Set the no button label.
	if the text is nil, remove the button."

	aStringOrText
		ifNil: [self noButton delete]
		ifNotNil: [self noButton
					hResizing: #shrinkWrap;
					label: aStringOrText;
					setBalloonText: helpString]