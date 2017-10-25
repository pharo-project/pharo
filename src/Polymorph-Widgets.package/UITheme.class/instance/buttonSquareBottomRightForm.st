fill-styles
buttonSquareBottomRightForm
	"Answer the form to use for the bottom right of a square button."

	^self forms at: #buttonSquareBottomRight ifAbsent: [Form extent: 12@12 depth: Display depth]