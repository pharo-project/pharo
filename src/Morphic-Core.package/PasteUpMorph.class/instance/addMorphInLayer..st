wiw support
addMorphInLayer: aMorph
	super addMorphInLayer: aMorph.
	aMorph wantsToBeTopmost ifFalse:[self bringTopmostsToFront].