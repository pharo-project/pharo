instance creation
withAll: someRefactorings
	^ self basicNew 	
		initializeWith: someRefactorings;
		yourself