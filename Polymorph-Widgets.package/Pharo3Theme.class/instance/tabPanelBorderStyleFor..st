border-styles
tabPanelBorderStyleFor: aTabGroup
	^ Pharo3TabPanelBorder new 
		width: 1;
		baseColor: ((self glamorousDarkBaseColorFor: aTabGroup));
		tabSelector: aTabGroup tabSelectorMorph