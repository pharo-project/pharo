fill-styles-buttons
buttonSelectedFillStyleFor: aButton
	"Return the selected button fillStyle for the given button."
	
	^aButton isDefault 
		ifTrue: [self buttonPlainFillStyleFor: aButton]
		ifFalse: [self buttonColouredFillStyleFor: aButton]