announcing
valueAdded: newValue
	self announcer announce: (ValueAdded newValue: newValue).
	self valueChanged: newValue