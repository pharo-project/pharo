label-styles
windowMinimizeOverForm
	"Answer the form to use for mouse over window minimize buttons"

	^self forms at: #windowMinimizeOver ifAbsent: [Form extent: 16@16 depth: Display depth]