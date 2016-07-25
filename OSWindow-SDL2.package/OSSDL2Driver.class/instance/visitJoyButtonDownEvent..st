global events
visitJoyButtonDownEvent: joyEvent
	^ OSJoyButtonDownEvent new
		which: joyEvent which;
		button: joyEvent button;
		hasMapping: (self isGameController: joyEvent which);
		mapped: false;
		pressed: true