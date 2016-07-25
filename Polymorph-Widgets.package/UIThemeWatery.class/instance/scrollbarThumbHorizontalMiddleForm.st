fill-styles
scrollbarThumbHorizontalMiddleForm
	"Answer the form to use for the middle of a horizontal scrollbar."

	^self forms at: #sbHThumbMiddle ifAbsent: [Form extent: 1@13 depth: Display depth]