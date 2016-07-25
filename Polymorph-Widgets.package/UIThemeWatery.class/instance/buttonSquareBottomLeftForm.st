fill-styles
buttonSquareBottomLeftForm
	"Answer the form to use for the bottom left of a square button."

	^self forms at: #buttonSquareBottomLeft ifAbsent: [Form extent: 12@12 depth: Display depth]