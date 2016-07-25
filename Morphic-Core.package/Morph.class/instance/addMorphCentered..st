submorphs-add/remove
addMorphCentered: aMorph

	aMorph position: bounds center - (aMorph extent // 2).
	self addMorphFront: aMorph.
