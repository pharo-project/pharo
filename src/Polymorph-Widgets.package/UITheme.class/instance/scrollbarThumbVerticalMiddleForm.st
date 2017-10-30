fill-styles
scrollbarThumbVerticalMiddleForm
	"Answer the form to use for the middle of a vertical scrollbar."

	^self forms at: #sbVThumbMiddle ifAbsent: [Form extent: 13@1 depth: Display depth]