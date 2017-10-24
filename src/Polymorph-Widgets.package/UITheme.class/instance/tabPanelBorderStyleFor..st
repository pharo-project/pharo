border-styles
tabPanelBorderStyleFor: aTabGroup
	"Answer the normal border style for a tab group."

	^TabPanelBorder new
		width: 1;
		baseColor: (aTabGroup paneColor alphaMixed: 0.8 with: Color black);
		tabSelector: aTabGroup tabSelectorMorph