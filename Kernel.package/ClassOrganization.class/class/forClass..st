instance creation
forClass: aClass
	^ self basicNew
		initializeClass: aClass;
		yourself