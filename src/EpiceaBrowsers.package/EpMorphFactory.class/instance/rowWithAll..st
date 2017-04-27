generic
rowWithAll: someMophs

	^ Morph new
		changeTableLayout;
		color: Color transparent;
		vResizing: #shrinkWrap;
		hResizing: #shrinkWrap;
		listDirection: #leftToRight;
		cellInset: 2;
		addAllMorphs: someMophs;
		yourself