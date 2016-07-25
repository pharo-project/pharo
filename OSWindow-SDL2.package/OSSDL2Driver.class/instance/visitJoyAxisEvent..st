global events
visitJoyAxisEvent: joyEvent
	^ OSJoyAxisEvent new
		which: joyEvent which;
		axis: joyEvent axis;
		hasMapping: (self isGameController: joyEvent which);
		mapped: false;
		value: joyEvent value