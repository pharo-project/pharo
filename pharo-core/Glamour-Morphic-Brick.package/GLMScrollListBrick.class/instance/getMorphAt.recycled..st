lazy scrolling
getMorphAt: anInteger recycled: aMorph
	|newMorph|
	
	newMorph := (self model morphAt: anInteger convert: aMorph) asBrick.
	newMorph hSpaceFill.
	self setIndex: anInteger in: newMorph.
	^ newMorph