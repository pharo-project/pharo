build ui
buildButtonsBar
	| arrows buttons |
	arrows := PanelMorph new
		changeProportionalLayout;
		addMorph: self buildNavigationPreviousButton fullFrame: (0 @ 0 corner: 0.5 @ 1) asLayoutFrame;
		addMorph: self buildNavigationNextButton fullFrame: (0.5 @ 0 corner: 1 @ 1) asLayoutFrame;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself.
	buttons := PanelMorph new
		changeProportionalLayout;
		addMorph: self buildBrowseScopedButton
			fullFrame: ((0 @ 0 corner: 0.1 @ 1) asLayoutFrame topLeftOffset: 2 @ 0);
		addMorph: self buildBrowseVariablesButton
			fullFrame: ((0.1 @ 0 corner: 0.2 @ 1) asLayoutFrame topLeftOffset: 2 @ 0);
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself.
	^ PanelMorph new
		changeProportionalLayout;
		addMorph: buttons
			fullFrame:
			(LayoutFrame identity
				leftFraction: 0;
				yourself);
		addMorph: self buildNavigationList
			fullFrame:
			(LayoutFrame identity
				leftFraction: 0.5;
				rightFraction: 0.95;
				yourself);
		addMorph: arrows
			fullFrame:
			(LayoutFrame identity
				leftFraction: 0.95;
				yourself);
		yourself