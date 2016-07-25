build ui lists
buildFirstColumn: aWindow
	^ PanelMorph new
		changeProportionalLayout;
		addMorph: self buildPackageSearch
			fullFrame:
			(LayoutFrame identity
				bottomFraction: 0;
				bottomOffset: StandardFonts defaultFont pixelSize + 10;
				leftOffset: -1;
				rightOffset: 1);
		addMorph: self buildPackageWidget
			fullFrame: (LayoutFrame identity topOffset: StandardFonts defaultFont pixelSize + 14);
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself