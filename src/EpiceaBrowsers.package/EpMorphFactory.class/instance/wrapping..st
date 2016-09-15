strings
wrapping: aMorphOrString

	^ PanelMorph new
		changeTableLayout;
		addMorphBack: aMorphOrString asMorph;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		layoutInset: 2@0;
		yourself