fill-styles
scrollbarThumbRightForm
	"Answer the form to use for the right of a horizontal scrollbar."

	^self forms at: #sbHThumbRight ifAbsent: [Form extent: 7@13 depth: Display depth]