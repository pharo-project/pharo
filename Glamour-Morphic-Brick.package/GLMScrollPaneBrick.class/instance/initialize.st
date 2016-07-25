initialization
initialize
	super initialize.
	
	lastScrollTimeStamp := 0.

	self useVerticalLinearLayout.
	self band: self newBand.
	self verticalScrollbar: self newVerticalScrollbar.	
	"initializes listener for mouse wheel scroll on windows vm"
	self initializeListeners