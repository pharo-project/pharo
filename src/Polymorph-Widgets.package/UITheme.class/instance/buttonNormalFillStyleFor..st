fill-styles-buttons
buttonNormalFillStyleFor: aButton
	"Return the normal button fillStyle for the given button."
	
	^aButton isDefault 
		ifTrue: [self buttonColouredFillStyleFor: aButton]
		ifFalse: [self buttonPlainFillStyleFor: aButton]