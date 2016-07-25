event handling
selectNextItem
	"Select the next item in the embedded menus"

	| idx next |
	self choiceMenus 
		do: [:embeddedMenu | embeddedMenu menuItems 
			do: [:mi | ((mi isMenuItemMorph)  and: [mi isSelected])
				ifTrue: [idx := embeddedMenu menuItems indexOf: mi.
					idx = embeddedMenu menuItems size
						ifTrue: [idx := 0].
					idx := idx + 1. 
					[(embeddedMenu menuItems at: idx) isMenuItemMorph]
						whileFalse: [idx := idx+ 1].
					next := embeddedMenu menuItems at: idx.
					self activeHand newKeyboardFocus: self.
					self scrollPane ifNotNil: [:sp | sp scrollToShow: next bounds].
					^ embeddedMenu selectItem: next event: nil]]].
	self selectFirstItem