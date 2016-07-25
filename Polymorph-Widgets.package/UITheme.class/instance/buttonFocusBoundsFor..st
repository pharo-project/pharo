border-styles-buttons
buttonFocusBoundsFor: aButton
	"Answer the bounds for drawing the focus indication for the
	given button."

	^(aButton bounds extent min < 6)
		ifTrue: [aButton bounds]
		ifFalse: [aButton bounds insetBy: aButton borderWidth + 
					(aButton label isMorph ifTrue: [1] ifFalse: [2])]