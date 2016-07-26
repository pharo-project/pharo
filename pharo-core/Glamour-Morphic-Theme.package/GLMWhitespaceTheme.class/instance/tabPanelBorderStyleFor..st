border-styles
tabPanelBorderStyleFor: aTabGroup
	^ GLMTabPanelBorder new 
		width: 1;
		baseColor: (self glamorousLightColorFor: aTabGroup);
		tabSelector: aTabGroup tabSelectorMorph