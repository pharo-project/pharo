build ui
setFastTableLayoutFrame: newTableMorph
	newTableMorph layoutFrame: (LayoutFrame identity 
		fractions: (0 @ 0 corner: 1 @ 1);
		yourself)