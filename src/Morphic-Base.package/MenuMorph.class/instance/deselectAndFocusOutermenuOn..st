control
deselectAndFocusOutermenuOn: anEvent
	"deselect and return focus to outer menu"

	self selectItem: nil event: anEvent.
	anEvent hand newMouseFocus: popUpOwner owner.
	^ anEvent hand newKeyboardFocus: popUpOwner owner