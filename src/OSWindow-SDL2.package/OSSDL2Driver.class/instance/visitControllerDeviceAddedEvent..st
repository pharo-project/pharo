global events
visitControllerDeviceAddedEvent: controllerEvent
	self openAddedController: controllerEvent which.
	^ OSJoyDeviceAddedEvent new
		which: controllerEvent which;
		mapped: true;
		hasMapping: true