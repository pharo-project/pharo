label-styles
windowMenuForm
	"Answer the form to use for the menu button of a window."

	^self forms at: #windowMenu ifAbsent: [Form extent: 10@10 depth: Display depth]