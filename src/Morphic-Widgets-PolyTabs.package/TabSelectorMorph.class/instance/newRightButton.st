operations
newRightButton
	"Answer a new button for scrolling right."

	^(ControlButtonMorph
			on: self
			getState: nil
			action: #scrollTabsRight
			label: #rightButtonLabel)
		getEnabledSelector: #rightButtonEnabled;
		hResizing: #rigid;
		vResizing: #spaceFill;
		cornerStyle: #square;
		extent: self controlButtonWidth asPoint