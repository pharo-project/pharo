fill-styles
buttonSelectedMiddleLeftForm
	"Answer the form to use for the middle left of a selected button."

	^self forms at: #buttonSelectedMiddleLeft ifAbsent: [Form extent: 12@1 depth: Display depth]