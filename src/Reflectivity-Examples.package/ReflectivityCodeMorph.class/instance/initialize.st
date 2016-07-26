initialization
initialize

	super initialize.
	self
 		layoutPolicy: TableLayout new;
		listDirection: #leftToRight;
		wrapDirection: #topToBottom;
		hResizing: #spaceFill;
		vResizing: #rigid;
		height: 150;
		layoutInset: (Margin top: 30 left: 2 bottom: 2 right: 2); 
		cellInset: 1.
		