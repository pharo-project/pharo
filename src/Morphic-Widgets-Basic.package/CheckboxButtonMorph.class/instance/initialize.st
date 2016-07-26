initialization
initialize
	"Initialize the receiver."

	super initialize.
	self
		isRadioButton: false;
		enabled: true;
		onImage: self theme checkboxMarkerForm;
		fillStyle: self fillStyleToUse;
		borderStyle: self borderStyleToUse