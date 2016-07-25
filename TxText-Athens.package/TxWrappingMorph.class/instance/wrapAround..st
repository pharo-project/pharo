accessing
wrapAround: aMorph
	aMorph delete.
	aMorph position: 0@0.
	self addMorph: aMorph.
	
	self extent: aMorph extent.