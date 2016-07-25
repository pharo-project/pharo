label-styles
windowMenuOverForm
	"Answer the form to use for mouse over window menu buttons"

	^self forms at: #windowMenuOver ifAbsent: [Form extent: 16@16 depth: Display depth]