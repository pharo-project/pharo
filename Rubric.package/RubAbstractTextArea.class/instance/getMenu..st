event handling
getMenu: shiftKeyState
	"Ask the getMenuPolicy to lookup the menu to popup if any"

	^ shiftKeyState
		ifTrue: [ self getMenuPolicy lookupShiftMenu ]
		ifFalse: [ self getMenuPolicy lookupMenu ]