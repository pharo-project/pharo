menu
getMenu: shiftKeyState
	"Answer the menu for this text view, supplying an empty menu to be filled in. If the menu selector takes an extra argument, pass in the current state of the shift key."

	| aMenu |
	aMenu := super getMenu: shiftKeyState.
	aMenu ifNotNil: [aMenu commandKeyHandler: self].
	^ aMenu