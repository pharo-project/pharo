instance creation
event: event from: source

	^ self new
		event: event;
		source: source;
		yourself