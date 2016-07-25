*Morphic-Widgets-Windows
addCornerGrips
	self addMorphBack: (TopLeftGripMorph new target: self; position: self position).
	self addMorphBack: (TopRightGripMorph new target: self; position: self position).
	self addMorphBack: (BottomLeftGripMorph new target: self; position: self position).
	self addMorphBack: (BottomRightGripMorph new target: self; position: self position)