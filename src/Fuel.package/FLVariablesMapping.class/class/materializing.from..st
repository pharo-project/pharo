instance creation
materializing: aClass from: aDecoder
	
	^ self basicNew
		initializeWithClass: aClass;
		initializeMaterializingFrom: aDecoder;
		yourself.