*Polymorph-Widgets
addEdgeGrips
	"Should add these to the front!"
	
	|t l r lh|
	lh := self labelHeight.
	t  := WindowEdgeGripMorph new target: self; position: self position; edgeName: #top.
	t layoutFrame
		topOffset: lh negated;
		bottomOffset: lh negated + self class borderWidth.
	l  := WindowEdgeGripMorph new target: self; position: self position; edgeName: #left.
	l layoutFrame topOffset: lh negated + 22.
	r  := WindowEdgeGripMorph new target: self; position: self position; edgeName: #right.
	r layoutFrame topOffset: lh negated + 22.
	self
		addMorph: t;
		addMorph: l;
		addMorph: r;
		addMorph: (WindowEdgeGripMorph new target: self;position: self position; edgeName: #bottom)