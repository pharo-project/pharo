fill-styles
buttonSelectedMiddleRightForm
	"Answer the form to use for the middle right of a selected button."

	^self forms at: #buttonSelectedMiddleRight ifAbsent: [Form extent: 12@1 depth: Display depth]