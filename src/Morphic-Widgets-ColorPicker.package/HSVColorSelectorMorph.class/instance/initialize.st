initialization
initialize
	"Initialize the receiver."

	super initialize.
	self
		borderWidth: 0;
		changeTableLayout;
		cellInset: 4;
		listDirection: #leftToRight;
		cellPositioning: #topLeft;
		svMorph: self newSVColorMorph;
		hMorph: self newHColorMorph;
		addMorphBack: self svMorph;
		addMorphBack: self hMorph;
		extent: 192@152;
		hue: 0.5