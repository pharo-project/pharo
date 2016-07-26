initialization
initialize
	super initialize.
	self convertTo: String.
	list := OrderedCollection new.
	self useSelectionIndex: false.
	self ghostText: 'Hit return to accept' translated