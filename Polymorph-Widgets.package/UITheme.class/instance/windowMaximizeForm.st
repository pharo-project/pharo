label-styles
windowMaximizeForm
	"Answer the form to use for the maximize button of a window."

	^self forms at: #windowMaximize ifAbsent: [Form extent: 10@10 depth: Display depth]