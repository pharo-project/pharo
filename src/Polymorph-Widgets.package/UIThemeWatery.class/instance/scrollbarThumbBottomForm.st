fill-styles
scrollbarThumbBottomForm
	"Answer the form to use for the bottom of a vertical scrollbar."

	^self forms at: #sbVThumbBottom ifAbsent: [Form extent: 13@7 depth: Display depth]