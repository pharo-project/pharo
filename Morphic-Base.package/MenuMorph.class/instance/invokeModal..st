modal control
invokeModal: allowKeyboardControl
	"Invoke this menu and don't return until the user has chosen a value.  If the allowKeyboarControl boolean is true, permit keyboard control of the menu"

	^ self invokeModalAt: ActiveHand position in: ActiveWorld allowKeyboard: allowKeyboardControl