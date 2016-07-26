label-styles
windowCloseOverForm
	"Answer the form to use for mouse over window close buttons"

	^self forms at: #windowCloseOver ifAbsent: [Form extent: 16@16 depth: Display depth]