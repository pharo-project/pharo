instance creation
handle: aHandle offset: aNumber
	^ self basicNew 
		initializeHandle: aHandle offset: aNumber;
		yourself