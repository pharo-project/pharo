initialization
initialize
	"Initialize the receiver."

	super initialize.
	self locationMorph: self newLocationMorph.
	self
		clipSubmorphs: true;
		color: Color blue;
		borderStyle: (BorderStyle inset width: 1);
		addMorphBack: self locationMorph;
		addMorphBack: self blackGradientMorph