initialization
intoWorld: aWorld
	"Stick to the bottom left now."
	
	self
		setToAdhereToEdge: #bottomLeft;
		updateBounds.
	super intoWorld: aWorld