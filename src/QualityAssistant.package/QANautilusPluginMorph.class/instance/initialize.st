initialization
initialize
	super initialize.
	
	critiques := #().
	
	self
		layoutPolicy: TableLayout new;
		listDirection: #leftToRight;
		cellPositioning: #center;
		hResizing: #shrinkWrap;
		vResizing: #rigid;
		height: 51; "because it fits 3 row list without showing scrollbars"
		cellInset: 3;
		color: Color transparent.
	
	self initializeList.
	self initializeProgress.
	self initializeUpdateTask.
	
	self addMorphBack: listMorph.
	
	self subscribe