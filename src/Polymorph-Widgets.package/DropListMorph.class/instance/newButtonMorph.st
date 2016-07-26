drawing
newButtonMorph
	"Answer a new button morph"

	^(ControlButtonMorph
		on: self
		getState: nil
		action: #popList
		label: #buttonLabel)
			roundedCorners: #(3 4);
			getEnabledSelector: #enabled;
			label: self buttonLabel; 
			vResizing: #spaceFill;
			hResizing: #rigid;
			extent: self buttonExtent;
			setProperty: #wantsKeyboardFocusNavigation toValue: false;
			cornerStyle: self cornerStyle.