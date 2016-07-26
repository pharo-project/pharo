menu commands
acceptTextInModelBasic
	"Inform the model that the receiver's textMorph's text should be accepted.
	Answer true if the model accepted ok, false otherwise"
	
	| textToAccept |
	textToAccept := textMorph asText.
	^setTextSelector isNil or:
		[setTextSelector numArgs = 2
			ifTrue: [model perform: setTextSelector with: textToAccept with: self]
			ifFalse: [model perform: setTextSelector with: textToAccept]]
