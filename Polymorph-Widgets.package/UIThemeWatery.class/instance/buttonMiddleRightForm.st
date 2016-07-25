fill-styles
buttonMiddleRightForm
	"Answer the form to use for the middle right of a button."

	^self forms at: #buttonMiddleRight ifAbsent: [Form extent: 11@1 depth: Display depth]