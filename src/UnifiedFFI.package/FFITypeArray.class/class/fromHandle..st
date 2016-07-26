instance creation
fromHandle: aHandle
	^ self basicNew 
		initializeHandle: aHandle type: self type size: self numberOfElements;
		yourself