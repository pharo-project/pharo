world state
restoreMorphicDisplay

	DisplayScreen startUp.

	self
		extent: Display extent;
		viewBox: Display boundingBox;
		handsDo: [:h | h visible: true; showTemporaryCursor: nil];
		resizeBackgroundMorph;
		submorphsDo: [ :each | each displayExtentChanged ];
		fullRepaintNeeded.
		
	self defer: [Cursor normal show].
		
	self layoutChanged.
