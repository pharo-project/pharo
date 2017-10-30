accessing
valueChanged: oldValue to: newValue
	
	self announcer announce: (ValueChanged oldValue: oldValue newValue: newValue)