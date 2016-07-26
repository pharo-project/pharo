instance creation
fromBehavior: aClass newName: aName 

	^ self basicNew
		initializeWithBehavior: aClass 
		newName: aName;
		yourself