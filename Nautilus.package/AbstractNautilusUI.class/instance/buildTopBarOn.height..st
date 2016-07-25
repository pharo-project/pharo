build ui
buildTopBarOn: aWindow height: topHeight
	aWindow
		addMorph: self buildButtonsBar
		fullFrame:
			(LayoutFrame identity
				topOffset: topHeight;
				bottomFraction: 0;
				bottomOffset: self buttonsBarHeight;
				leftOffset: -1;
				rightOffset: 1)