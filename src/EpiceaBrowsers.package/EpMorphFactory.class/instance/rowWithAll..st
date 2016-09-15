generic
rowWithAll: someMophs
	^ PanelMorph new
		changeTableLayout;
		vResizing: #shrinkWrap;
		hResizing: #shrinkWrap;
		listDirection: #leftToRight;
		cellInset: 2;
		addAllMorphs: someMophs;
		yourself