submorphs-add/remove
addMorph: aMorph fullFrame: aLayoutFrame

	aMorph layoutFrame: aLayoutFrame asLayoutFrame.
	aMorph hResizing: #spaceFill; vResizing: #spaceFill.
	self addMorph: aMorph.

