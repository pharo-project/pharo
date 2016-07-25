*Polymorph-Widgets
addCornerGrips
	"Should add these to the front!"
	
	|tl tr lh|
	lh := self labelHeight.
	tl  := TopLeftGripMorph new target: self; position: self position.
	tl layoutFrame topOffset: lh negated.
	tr  := TopRightGripMorph new target: self; position: self position.
	tr layoutFrame topOffset: lh negated.
	self
		addMorph: tl;
		addMorph: tr;
		addMorph: (BottomLeftGripMorph new target: self;position: self position);
		addMorph: (BottomRightGripMorph new target: self;position: self position)