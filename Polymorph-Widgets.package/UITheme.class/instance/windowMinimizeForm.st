label-styles
windowMinimizeForm
	"Answer the form to use for the minimize button of a window."

	^self forms at: #windowMinimize ifAbsent: [Form extent: 10@10 depth: Display depth]