initialization
initialize
	super initialize.
	self 
		clipLayoutCells: true;
		color: Color transparent.
	progress := 1.
	progressIconMorph := self currentIcon asMorph.
	self addMorphBack: progressIconMorph.
	self extent: progressIconMorph extent.