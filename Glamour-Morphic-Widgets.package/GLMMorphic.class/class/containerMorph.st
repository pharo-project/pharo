utils - morph creation
containerMorph
	^ PanelMorph new
		layoutPolicy: ProportionalLayout new;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		borderWidth: 0;
		yourself