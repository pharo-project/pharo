switching
toggleState
	self isOn
		ifTrue: [self turnOff]
		ifFalse: [self turnOn]