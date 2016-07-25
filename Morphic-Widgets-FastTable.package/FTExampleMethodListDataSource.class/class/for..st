instance creation
for: aClass
	^ self basicNew 
		initializeClass: aClass;
		yourself