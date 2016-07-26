initialization
initialize
	
	"initialize the state of the receiver"
	super initialize.
	hasFocus := false.
	self initializeScrollBars.
	self extent: self defaultExtent.
	self hideOrShowScrollBars.


