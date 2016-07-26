menu & halo
addWorldHaloMenuItemsTo: aMenu hand: aHandMorph
	"Add standard halo items to the menu, given that the receiver is a World"

	self addFillStyleMenuItems: aMenu hand: aHandMorph.
	self addLayoutMenuItems: aMenu hand: aHandMorph.

	aMenu addLine.
	self addWorldToggleItemsToHaloMenu: aMenu.
	aMenu addLine.
	self addExportMenuItems: aMenu hand: aHandMorph.
	self addMiscExtrasTo: aMenu.
	self addDebuggingItemsTo: aMenu hand: aHandMorph.

	aMenu addLine.
	aMenu defaultTarget: aHandMorph.
