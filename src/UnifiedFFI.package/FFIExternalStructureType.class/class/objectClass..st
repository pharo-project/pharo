instance creation
objectClass: aClass
	^ self basicNew 
		initializeObjectClass: aClass;
		yourself