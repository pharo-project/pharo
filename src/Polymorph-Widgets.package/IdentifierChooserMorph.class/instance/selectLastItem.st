event handling
selectLastItem
	"Select the last item in the embedded menus"

	self choiceMenus last selectItem:  self choiceMenuItems last event: nil.
	self activeHand newKeyboardFocus: self.
