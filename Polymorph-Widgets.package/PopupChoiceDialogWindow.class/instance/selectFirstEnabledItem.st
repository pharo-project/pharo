as yet unclassified
selectFirstEnabledItem
	"Select the first enabled item in any of the embedded menus"

	|found|
	found := false.
	(self choiceMenus ifNil: [^self]) do: [:embeddedMenu |
		embeddedMenu selectItem: nil event: nil]. "clear selection in other menus"
	self choiceMenus do: [:embeddedMenu | 
		(embeddedMenu selectMatch: self filter)
			ifNotNil: [:menuItem |
				found ifFalse: [
					embeddedMenu selectItem: menuItem event: nil.
					self activeHand newKeyboardFocus: embeddedMenu.
					found := true]]]