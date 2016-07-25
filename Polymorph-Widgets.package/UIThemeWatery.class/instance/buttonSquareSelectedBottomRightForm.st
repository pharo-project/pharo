fill-styles
buttonSquareSelectedBottomRightForm
	"Answer the form to use for the bottom right of a selected square button."

	^self forms at: #buttonSquareSelectedBottomRight ifAbsent: [Form extent: 12@12 depth: Display depth]