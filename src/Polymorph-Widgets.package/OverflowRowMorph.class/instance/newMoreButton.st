instance creation
newMoreButton
	"Answer a new button for popping up the base morphs that are currently
	unable to fit in the allotted space."

	^(ControlButtonMorph
			on: self
			getState: nil
			action: #popMore
			label: #moreButtonLabel)
		hResizing: #rigid;
		vResizing: #spaceFill;
		cornerStyle: #square;
		extent: self buttonWidth asPoint