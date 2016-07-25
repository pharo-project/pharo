morph creation
newToolbarIn: aThemedMorph
	"Answer a new toolbar."

	|bar|
	bar := PanelMorph new
		changeTableLayout;
		layoutInset: 0@1;
		listDirection: #leftToRight;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap.
	bar
		addMorphBack: (self newToolSpacerIn: aThemedMorph);
		addMorphBack: (self newToolbarHandleIn: aThemedMorph);
		addMorphBack: (self newToolSpacerIn: aThemedMorph).
	^bar