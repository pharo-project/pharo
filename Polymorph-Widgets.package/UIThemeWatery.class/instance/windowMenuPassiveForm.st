label-styles
windowMenuPassiveForm
	"Answer the form to use for inactive window menu buttons"

	^self forms at: #windowMenuPassive ifAbsent: [Form extent: 16@16 depth: Display depth]