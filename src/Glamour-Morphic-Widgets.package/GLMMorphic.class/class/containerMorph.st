utils - morph creation
containerMorph
	^ GLMPanelMorph new
		layoutPolicy: ProportionalLayout new;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		borderWidth: 0;
		yourself