build ui
buildColumns: aWindow height: height
	aWindow
		addMorph: (self buildFirstColumn: aWindow)
		fullFrame:
			((0 @ 0 corner: 0.22 @ self navigationAreaProportion) asLayoutFrame topOffset: height).
	aWindow
		addMorph: (self buildSecondColumn: aWindow)
		fullFrame:
			((0.22 @ 0 corner: 0.50 @ self navigationAreaProportion) asLayoutFrame topOffset: height).
	aWindow
		addMorph: (self buildThirdColumn: aWindow)
		fullFrame:
			((0.50 @ 0 corner: 0.72 @ self navigationAreaProportion) asLayoutFrame topOffset: height).
	aWindow
		addMorph: (self buildFourthColumn: aWindow)
		fullFrame:
			((0.72 @ 0 corner: 1 @ self navigationAreaProportion) asLayoutFrame topOffset: height)