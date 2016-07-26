control
popUpNoKeyboard
	"Present this menu in the current World, *not* allowing keyboard input into the menu"

	^ self popUpAt: ActiveHand position forHand: ActiveHand in: ActiveWorld allowKeyboard: false