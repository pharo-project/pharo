signalling
signalWith: anError
	^ self new
		messageText: anError message;
		object: anError type;
		signal;
		yourself