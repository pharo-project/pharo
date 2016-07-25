global events
visitJoyButtonUpEvent: joyEvent
	^ OSJoyButtonUpEvent new
		which: joyEvent which;
		button: joyEvent button;
		hasMapping: (self isGameController: joyEvent which);
		mapped: false;
		pressed: false