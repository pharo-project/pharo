grips
addEdgeGrips
	"Should add these to the front!"
	
	| l r lh |
	lh := self labelHeight.
	l  := WindowEdgeGripMorph new target: self; position: self position; edgeName: #left.
	l layoutFrame topOffset: lh negated + 22.
	r  := WindowEdgeGripMorph new target: self; position: self position; edgeName: #right.
	r layoutFrame topOffset: lh negated + 22.
	self
		addMorph: l;
		addMorph: r
