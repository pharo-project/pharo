instance creation
method: aCompiledMethod

	^ self basicNew
		initializeWithMethod: aCompiledMethod;
		yourself