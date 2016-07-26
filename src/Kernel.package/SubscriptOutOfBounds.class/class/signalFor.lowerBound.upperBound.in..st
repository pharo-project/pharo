signaling
signalFor: subscript lowerBound: lowerBound upperBound: upperBound in: object
	^ self new
		signaler: object;
		subscript: subscript;
		lowerBound: lowerBound;
		upperBound: upperBound;
		signal		 