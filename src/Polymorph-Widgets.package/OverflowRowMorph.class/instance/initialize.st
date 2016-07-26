initialization
initialize
	"Initialize the receiver."

	super initialize.
	self
		baseMorphs: #();
		moreButton: self newMoreButton;
		changeTableLayout;
		listDirection: #leftToRight;
		vResizing: #shrinkWrap