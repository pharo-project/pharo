menu
addCustomMenuItems: aCustomMenu hand: aHandMorph

	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu add: 'change font' selector: #changeFont.
	aCustomMenu add: 'change emphasis' selector: #changeEmphasis.
