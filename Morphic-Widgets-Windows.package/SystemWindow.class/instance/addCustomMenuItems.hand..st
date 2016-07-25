menu actions
addCustomMenuItems: aCustomMenu hand: aHandMorph
	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
"template..."
	aCustomMenu addLine.
	aCustomMenu add: 'Edit label...' selector: #relabel.
