user interface
emptyList
	self list removeAll.
	self value ifNotNil: [self addToList: self realValue].