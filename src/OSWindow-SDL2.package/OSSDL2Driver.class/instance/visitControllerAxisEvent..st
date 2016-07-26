global events
visitControllerAxisEvent: joyEvent
	^ OSJoyAxisEvent new
		which: joyEvent which;
		axis: joyEvent axis;
		mapped: true;
		hasMapping: true;
		value: joyEvent value