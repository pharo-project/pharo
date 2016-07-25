updating
scrollBarMenuButtonPressed: event
	"The menu button in the scrollbar was pressed; put up the menu"

	| menu |
	(menu := self getMenu: event shiftPressed) ifNotNil:
		["Set up to use perform:orSendTo: for model/view dispatch"
		menu setInvokingView: self.
		menu invokeModal]