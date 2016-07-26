initialization
initialize
	super initialize.
	
	vResizing := #spaceFill.
	hResizing := #spaceFill.
	
	self
		beTransparent;
		brickRoot: GLMBrick new