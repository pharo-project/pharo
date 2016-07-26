event handling
selectFirstItem
	"Select the first item in the embedded menus"

	self choiceMenus first selectItem:  self choiceMenuItems first event: nil.
	self takeKeyboardFocus.