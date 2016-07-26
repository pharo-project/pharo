global events
visitControllerDeviceRemovedEvent: controllerEvent
	self closeRemovedController: controllerEvent which.
	^ OSJoyDeviceRemovedEvent new
		which: controllerEvent which;
		mapped: true;
		hasMapping: true