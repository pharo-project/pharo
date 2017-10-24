fill-styles
buttonSquareSelectedTopRightForm
	"Answer the form to use for the top right of a selected square button."

	^self forms at: #buttonSquareSelectedTopRight ifAbsent: [Form extent: 12@12 depth: Display depth]