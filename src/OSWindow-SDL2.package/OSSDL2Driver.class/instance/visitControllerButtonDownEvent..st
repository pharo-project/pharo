global events
visitControllerButtonDownEvent: joyEvent
	^ OSJoyButtonDownEvent new
		which: joyEvent which;
		button: joyEvent button;
		mapped: true;
		hasMapping: true;
		pressed: true