fill-styles
buttonSquareSelectedTopLeftForm
	"Answer the form to use for the top left of a selected square button."

	^self forms at: #buttonSquareSelectedTopLeft ifAbsent: [Form extent: 12@12 depth: Display depth]