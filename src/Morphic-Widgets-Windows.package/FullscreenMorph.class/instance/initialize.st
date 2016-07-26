initialization
initialize
	"Initialize the receiver."

	super initialize.
	self
		changeProportionalLayout;
		bounds: World clearArea;
		beSticky