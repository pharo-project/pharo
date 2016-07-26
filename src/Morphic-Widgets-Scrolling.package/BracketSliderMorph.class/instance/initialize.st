initialization
initialize
	"Initialize the receiver."

	super initialize.
	self
		fillStyle: self defaultFillStyle;
		borderStyle: (BorderStyle inset baseColor: self paneColor; width: 1);
		sliderColor: Color black;
		clipSubmorphs: true