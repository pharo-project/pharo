build ui lists
buildFirstColumn: aWindow
	^ PanelMorph new
		changeProportionalLayout;
		addMorph: self buildPackageWidget
			fullFrame:
			(LayoutFrame identity bottomOffset: 1 - (StandardFonts defaultFont pixelSize + 10));
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself