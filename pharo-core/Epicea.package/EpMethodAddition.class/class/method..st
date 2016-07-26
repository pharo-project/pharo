instance creation
method: aCompiledMethod
	
	^ self basicNew
		initializeMethod: aCompiledMethod;
		yourself