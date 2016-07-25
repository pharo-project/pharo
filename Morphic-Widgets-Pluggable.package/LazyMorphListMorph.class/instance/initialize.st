initialization
initialize
	"Initialize the receiver."

	super initialize.
	self
		changeTableLayout;
		cellPositioning: #topLeft;
		cellInset: 2;
		vResizing: #shrinkWrap;
		hResizing: #rigid