accessing
valueChanged: oldValue
	
	self announcer announce: (ValueChanged oldValue: oldValue newValue: value)