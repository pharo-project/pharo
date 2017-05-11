instance creation
newForStore: aFileStore
	
	^ self basicNew
		initializeWithStore: aFileStore;
		yourself
