fill-styles
buttonMiddleLeftForm
	"Answer the form to use for the middle left of a button."

	^self forms at: #buttonMiddleLeft ifAbsent: [Form extent: 11@1 depth: Display depth]