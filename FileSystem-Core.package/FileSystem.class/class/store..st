instance creation
store: aStore
	^ self basicNew
		initializeWithStore: aStore;
		yourself