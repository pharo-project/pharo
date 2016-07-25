instance creation
oldMethod: anOldMethod newMethod: aNewMethod

	^ self basicNew
		initializeWithOldMethod: anOldMethod
		newMethod: aNewMethod;
		yourself