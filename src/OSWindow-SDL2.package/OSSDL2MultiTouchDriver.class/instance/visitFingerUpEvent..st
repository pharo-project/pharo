visitor
visitFingerUpEvent: event
	^OSTouchActionPointerUpEvent new
			timestamp: event timestamp;
			deviceId: event touchId;
			fingerId: event fingerId;
			position: event x @ event y;
			delta: event dx @ event dy;
			pressure: event pressure;
			yourself.
	