global events
visitDropEvent: dropEvent
	^ OSWindowDropEvent new
		timestamp: dropEvent timestamp;
		filePath: dropEvent file readString