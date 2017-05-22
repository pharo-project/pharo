signalling
signalWith: anObject
	^ self new
		object: anObject;
		signal;
		yourself