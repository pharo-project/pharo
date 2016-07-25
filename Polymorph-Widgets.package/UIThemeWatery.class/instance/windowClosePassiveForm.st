label-styles
windowClosePassiveForm
	"Answer the form to use for passive (background) window close buttons"

	^self forms at: #windowClosePassive ifAbsent: [Form extent: 16@16 depth: Display depth]