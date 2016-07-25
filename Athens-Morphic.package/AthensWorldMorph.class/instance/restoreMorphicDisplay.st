world state
restoreMorphicDisplay
"overwritten for using its own display dimension"
	DisplayScreen startUp.

	self
		extent: self display extent;
		viewBox: self display boundingBox;
		handsDo: [:h | h visible: true; showTemporaryCursor: nil];
		resizeBackgroundMorph;
		submorphsDo: [ :each | each displayExtentChanged ];
		fullRepaintNeeded.
		
	self defer: [Cursor normal show].
		
	self layoutChanged.
