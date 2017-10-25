fill-styles
buttonSquareTopLeftForm
	"Answer the form to use for the top left of a square button."

	^self forms at: #buttonSquareTopLeft ifAbsent: [Form extent: 12@12 depth: Display depth]