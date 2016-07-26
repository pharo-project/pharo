morph creation
newPanelIn: aThemedMorph
	"Answer a new panel."

	^PanelMorph new
		changeTableLayout;
		layoutInset: 4;
		cellInset: 8;
		cornerStyle: aThemedMorph preferredCornerStyle;
		yourself