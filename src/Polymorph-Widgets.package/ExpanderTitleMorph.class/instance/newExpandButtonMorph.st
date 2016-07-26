instance creation
newExpandButtonMorph
	"Answer a new expand button."

	^(ControlButtonMorph
			on: self
			getState: nil
			action: #toggleExpanded
			label: #expandLabel)
		hResizing: #rigid;
		vResizing: #spaceFill;
		cornerStyle: self cornerStyle;
		extent: self buttonWidth asPoint