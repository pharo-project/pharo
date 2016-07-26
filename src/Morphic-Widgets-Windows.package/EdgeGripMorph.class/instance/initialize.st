initialization
initialize
	"Initialize the receiver."
	
	super initialize.
	self
		fitTargetOwner: false;
		edgeName: #right;
		extent: self defaultWidth @ self defaultHeight;
		hResizing: #spaceFill;
		vResizing: #spaceFill