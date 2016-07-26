updating
updateWithEvent: anEvent
	self updateDelta: anEvent delta;
		addToMove: anEvent position;
		pressure: anEvent pressure;
		timestamp: anEvent timestamp