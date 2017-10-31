fill-styles
scrollbarThumbTopForm
	"Answer the form to use for the top of a vertical scrollbar."

	^self forms at: #sbVThumbTop ifAbsent: [Form extent: 13@7 depth: Display depth]