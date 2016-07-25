global events
visitJoyDeviceAddedEvent: joyEvent
	(SDL2 isGameController: joyEvent which) ifFalse: [ self openAddedJoystick: joyEvent which ].
	^ OSJoyDeviceAddedEvent new
		which: joyEvent which;
		hasMapping: (self isGameController: joyEvent which);
		mapped: false