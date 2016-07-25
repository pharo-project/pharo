instance creation
for: anEvent
	^ self new deviceId: anEvent deviceId;
		fingerId: anEvent fingerId;
		delta: anEvent delta;
		firstPosition: anEvent position;
		pressure: anEvent pressure;
		timestamp: anEvent timestamp