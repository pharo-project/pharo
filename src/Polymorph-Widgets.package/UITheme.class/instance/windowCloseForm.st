label-styles
windowCloseForm
	"Answer the form to use for the close button of a window."

	^self forms at: #windowClose ifAbsent: [Form extent: 10@10 depth: Display depth]