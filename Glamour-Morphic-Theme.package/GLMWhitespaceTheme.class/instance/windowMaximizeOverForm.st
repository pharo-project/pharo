label-styles
windowMaximizeOverForm
	"Answer the form to use for mouse over window maximize buttons"

	^self forms at: #windowMaximizeOver ifAbsent: [Form extent: 16@16 depth: Display depth]