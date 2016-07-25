global events
visitControllerButtonUpEvent: joyEvent
	^ OSJoyButtonUpEvent new
		which: joyEvent which;
		button: joyEvent button;
		mapped: true;
		hasMapping: true;
		pressed: false