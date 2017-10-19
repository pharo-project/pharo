announcing
valueRemoved: oldValue
	self announcer announce: (ValueRemoved oldValue: oldValue).
	self valueChanged: oldValue