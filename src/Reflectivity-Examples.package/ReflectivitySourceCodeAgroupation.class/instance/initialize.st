initialization
initialize
	super initialize.
	self
 		layoutPolicy: TableLayout new;
		hResizing: #spaceFill; 
		vResizing: #spaceFill;
		cellInset: 10.