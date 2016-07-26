private
doMenu: evt with: menuHandle
	"Ask hand to invoke the halo menu for my inner target."

	| menu |
	self obtainHaloForEvent: evt andRemoveAllHandlesBut: nil.
	target world displayWorld.
	menu := innerTarget buildHandleMenu: evt hand.
	innerTarget addTitleForHaloMenu: menu.
	menu popUpEvent: evt in: target world.
