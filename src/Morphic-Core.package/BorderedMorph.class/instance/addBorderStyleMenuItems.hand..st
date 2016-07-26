menu
addBorderStyleMenuItems: aMenu hand: aHandMorph
	"Add border-style menu items"

	| subMenu |
	subMenu := UIManager default newMenuIn: self for: self.
	subMenu add: 'border width...' selector: #changeBorderWidth:.
	subMenu add: 'border color...' selector: #changeBorderColor:.
	subMenu addLine.
	BorderStyle borderStyleChoices do:
		[:sym | (self borderStyleForSymbol: sym)
			ifNotNil:
				[subMenu add: sym target: self selector: #setBorderStyle: argument: sym]].
	aMenu add: 'border style' subMenu: subMenu
