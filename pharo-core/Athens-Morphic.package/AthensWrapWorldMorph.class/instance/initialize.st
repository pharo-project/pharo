initialization
initialize
	super initialize.
	self extent: self defaultExtent.
	self hResizing: #spaceFill.
	self vResizing: #spaceFill.
	wrappedWorldDisplay := AthensWrappedWorldDisplay on: self.
	worldmorph := AthensWorldMorph new.
	worldmorph color: Color orange.
	worldmorph addHand: HandMorph new.
	worldmorph display: wrappedWorldDisplay.
	self addMorph: worldmorph