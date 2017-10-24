initialization
initialize
	"Initialize the receiver."

	super initialize.
	self
		initializeForms;
		settings: self defaultSettings.
	self windowActiveDropShadowStyle: #diffuse