operations
newLeftButton
	"Answer a new button for scrolling left."

	^(ControlButtonMorph
			on: self
			getState: nil
			action: #scrollTabsLeft
			label: #leftButtonLabel)
		getEnabledSelector: #leftButtonEnabled;
		hResizing: #rigid;
		vResizing: #spaceFill;
		cornerStyle: #square;
		extent: self controlButtonWidth asPoint