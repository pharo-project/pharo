signalling
signalForException: anError

	^ self new
		exception: anError;
		signal