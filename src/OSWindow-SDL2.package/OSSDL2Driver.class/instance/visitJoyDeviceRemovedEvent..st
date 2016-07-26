global events
visitJoyDeviceRemovedEvent: joyEvent
	(SDL2 isGameController: joyEvent which) ifFalse: [ self closeRemovedJoystick: joyEvent which ].
	^ OSJoyDeviceRemovedEvent new
		which: joyEvent which;
		hasMapping: (self isGameController: joyEvent which);
		mapped: false