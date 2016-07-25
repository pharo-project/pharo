fill-styles
buttonSquareTopRightForm
	"Answer the form to use for the top right of a square button."

	^self forms at: #buttonSquareTopRight ifAbsent: [Form extent: 12@12 depth: Display depth]