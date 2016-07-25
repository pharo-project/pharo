event handling
addStandardHaloMenuItemsTo: aMenu hand: aHandMorph

	aMenu add: 'browse action' selector: #browseAction.
	aMenu addLine.
	super addStandardHaloMenuItemsTo: aMenu hand: aHandMorph.