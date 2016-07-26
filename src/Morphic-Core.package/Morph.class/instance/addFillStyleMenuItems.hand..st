menus
addFillStyleMenuItems: aMenu hand: aHand
	"Add the items for changing the current fill style of the Morph"
	| menu |
	menu := UIManager default newMenuIn: self for: self.
	self fillStyle addFillStyleMenuItems: menu hand: aHand from: self.
	menu addLine.
	menu add: 'solid fill' selector: #useSolidFill.
	menu add: 'gradient fill' selector: #useGradientFill.
	menu add: 'bitmap fill' selector: #useBitmapFill.
	menu add: 'default fill' selector: #useDefaultFill.
	aMenu add: 'fill style' subMenu: menu.
