initialization
initialize
	"Initialize the receiver."

	super initialize.
	self
		extent: 180@168;
		changeTableLayout;
		cellInset: 4;
		aMorph: self newAColorMorph;
		hsvMorph: self newHSVColorMorph;
		addMorphBack: self hsvMorph;
		addMorphBack: self aMorph.
	self aMorph color: self hsvMorph selectedColor