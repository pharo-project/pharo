control
popUpAt: aPoint forHand: hand in: aWorld
	"Present this menu at the given point under control of the given hand.  Allow keyboard input into the menu."

	^ self popUpAt: aPoint forHand: hand in: aWorld allowKeyboard: self menuKeyboardControl