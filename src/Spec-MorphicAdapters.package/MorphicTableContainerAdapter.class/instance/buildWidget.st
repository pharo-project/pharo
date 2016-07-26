factory
buildWidget
	^ PanelMorph new
		layoutPolicy: TableLayout new;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		yourself