fill-styles
buttonSquareSelectedBottomLeftForm
	"Answer the form to use for the bottom left of a selected square button."

	^self forms at: #buttonSquareSelectedBottomLeft ifAbsent: [Form extent: 12@12 depth: Display depth]