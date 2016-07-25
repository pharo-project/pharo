label-styles
windowMinimizePassiveForm
	"Answer the form to use for passive (background) window minimize buttons"

	^self forms at: #windowMinimizePassive ifAbsent: [Form extent: 16@16 depth: Display depth]