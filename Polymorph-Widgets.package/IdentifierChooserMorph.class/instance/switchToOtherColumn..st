event handling
switchToOtherColumn: aBlock
	"Give the next embedded menu keyboard focus. 
	The next menu indice is computed by the argument"

	| menuWithFocus idx menu sub subIdx |
	(self choiceMenus isNil or: [self choiceMenus isEmpty]) 
		ifTrue: [^self].
	menuWithFocus := self choiceMenus
		detect: [:m | m menuItems 
			anySatisfy: [:sm | 
				((sm isMenuItemMorph)  and: [sm isSelected]) 
					ifTrue: [sub := sm]. 
				sm isSelected]]
		ifNone: [].
	self choiceMenus do: [:embeddedMenu | embeddedMenu selectItem: nil event: nil].
	menuWithFocus isNil
		ifTrue: [self selectFirstItem]
		ifFalse: [
			idx := aBlock value: (self choiceMenus indexOf: menuWithFocus).
			menu := (self choiceMenus at: idx).
			subIdx := sub ifNil: [1] ifNotNil: [(menuWithFocus menuItems indexOf: sub) min: menu menuItems size].
			menu selectItem: (menu menuItems at: subIdx) event: nil.
			self scrollPane ifNotNil: [:sp | sp scrollToShow: menu bounds]].
	self activeHand newKeyboardFocus: self.