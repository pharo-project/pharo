announcing
valueRemoved: oldValue
	announcer announce: (ValueRemoved oldValue: oldValue).
	self valueChanged: oldValue