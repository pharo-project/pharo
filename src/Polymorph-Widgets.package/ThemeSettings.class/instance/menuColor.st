menu
menuColor
	"Answer the value of menuColor"

	^ menuColor ifNil: [self fallbackMenuColor]