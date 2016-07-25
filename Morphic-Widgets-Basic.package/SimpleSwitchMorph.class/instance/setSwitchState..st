switching
setSwitchState: aBoolean

	aBoolean
		ifTrue: [self turnOn]
		ifFalse: [self turnOff].
