accessing cursor
addCursor
	self cursor ifNil: [ self addMorph: (cursor := self newCursor) ]