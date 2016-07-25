initialization
initialize

	super initialize.
	
	self hResizing: #spaceFill.
	self vResizing: #spaceFill.
	splitsTopAndBottom := false.
	
	leftOrTop := OrderedCollection new.
	rightOrBottom := OrderedCollection new