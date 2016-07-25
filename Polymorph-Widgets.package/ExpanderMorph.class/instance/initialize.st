initialization
initialize
	"Initialize the receiver."

	super initialize.
	self
		changeTableLayout;
		listDirection: #topToBottom;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		titleMorph: self defaultTitleMorph;
		expanded: false