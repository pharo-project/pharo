fill-styles
scrollbarThumbLeftForm
	"Answer the form to use for the left of a horizontal scrollbar."

	^self forms at: #sbHThumbLeft ifAbsent: [Form extent: 7@13 depth: Display depth]