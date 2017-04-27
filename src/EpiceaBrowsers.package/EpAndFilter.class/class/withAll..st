instance creation
withAll: aCollection 
	^ self new 
		subFilters: aCollection;
		yourself