menus
addStandardHaloMenuItemsTo: aMenu hand: aHandMorph
	"Add standard halo items to the menu"

	aMenu add: 'send to back' selector: #goBehind.
	aMenu add: 'bring to front' selector: #comeToFront.
	self addEmbeddingMenuItemsTo: aMenu hand: aHandMorph.
	aMenu addLine.
	self addFillStyleMenuItems: aMenu hand: aHandMorph.
	self addBorderStyleMenuItems: aMenu hand: aHandMorph.
	self addDropShadowMenuItems: aMenu hand: aHandMorph.
	self addLayoutMenuItems: aMenu hand: aHandMorph.
	self addHaloActionsTo: aMenu.
	owner isTextMorph
		ifTrue: [ self addTextAnchorMenuItems: aMenu hand: aHandMorph ].
	aMenu addLine.
	self addToggleItemsToHaloMenu: aMenu.
	aMenu addLine.
	self addExportMenuItems: aMenu hand: aHandMorph.
	self addMiscExtrasTo: aMenu.
	self addDebuggingItemsTo: aMenu hand: aHandMorph.
	aMenu addLine.
	aMenu defaultTarget: aHandMorph