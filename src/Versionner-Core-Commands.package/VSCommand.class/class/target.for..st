instance creation
target: target for: requestor
	^ self new
		target: target;
		requestor: requestor;
		yourself