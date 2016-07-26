build ui lists
buildSecondColumn: aWindow
	| buttons buttonSize |
	buttonSize := StandardFonts defaultFont pixelSize + 10.
	buttons := PanelMorph new.
	buttons
		changeProportionalLayout;
		addMorph: self buildHierarchyButton fullFrame: ((0 @ 0 corner: 0.33 @ 0) asLayoutFrame bottomRightOffset: -2 @ buttonSize);
		addMorph: self buildInstanceButton fullFrame: ((0.33 @ 0 corner: 0.67 @ 0) asLayoutFrame bottomRightOffset: -2 @ buttonSize);
		addMorph: self buildCommentButton fullFrame: ((0.67 @ 0 corner: 1 @ 0) asLayoutFrame bottomRightOffset: -2 @ buttonSize);
		hResizing: #spaceFill;
		vResizing: #rigid;
		height: 25.
	^ PanelMorph new
		changeProportionalLayout;
		addMorph: self buildClassWidget fullFrame: (LayoutFrame identity bottomOffset: 1 - (buttonSize + 4));
		addMorph: buttons fullFrame: ((0 @ 1 corner: 1 @ 1) asLayoutFrame topOffset: 1 - (buttonSize + 1));
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself