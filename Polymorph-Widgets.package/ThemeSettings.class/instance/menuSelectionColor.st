menu
menuSelectionColor
	"Answer the value of menuSelectionColor"

	^menuSelectionColor ifNil: [self derivedMenuColor darker]