announcing
valueAdded: newValue
	announcer announce: (ValueAdded newValue: newValue).
	self valueChanged: newValue