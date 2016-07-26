label-styles
windowMaximizePassiveForm
	"Answer the form to use for passive (background) window maximize/restore buttons"

	^self forms at: #windowMaximizePassive ifAbsent: [Form extent: 16@16 depth: Display depth]