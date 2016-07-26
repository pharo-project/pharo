initialize
initialize
	super initialize.
	self fillStyle: self defaultFillStyle.
"	self borderStyle: self defaultBorderStyle."
	isOpen := false.
	self setLabel: 'Glamorous Watcher'.
	self addCornerGrips.
	self addEdgeGrips.
	self addContentsMorph.
	process := nil.
	self
		on: Character escape
		do: [ self hide ]