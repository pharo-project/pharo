private
newToolbarMorph
	^ PanelMorph new
		fillStyle: (SolidFillStyle color: Color transparent);
		borderStyle: (BorderStyle width: 0);
		vResizing: #spaceFill;
		hResizing: #shrinkWrap;
		cellPositioning: #center;
		changeTableLayout;
		cellInset: 0;
		yourself 