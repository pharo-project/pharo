global events
visitControllerDeviceRemappedEvent: controllerEvent
	^ OSJoyDeviceRemappedEvent new
		which: controllerEvent which;
		mapped: true;
		hasMapping: true