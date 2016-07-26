drop shadows
addDropShadowMenuItems: aMenu hand: aHand
	| menu |
	menu := UIManager default newMenuIn: self for: self.
	menu
		addUpdating: #hasDropShadowString
		selector: #toggleDropShadow.
	menu addLine.
	menu add: 'shadow color...' target: self selector: #changeShadowColor.
	menu add: 'shadow offset...' target: self selector: #setShadowOffset:.
	aMenu add: 'drop shadow' subMenu: menu.