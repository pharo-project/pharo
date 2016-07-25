basic-colors
buttonColorFor: aButton
	"Answer the colour for the given button."

	^self settings standardColorsOnly
		ifTrue: [self buttonColor]
		ifFalse: [ aButton colorToUse ]