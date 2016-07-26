instance creation
newWithStore: aStore 

	^ self basicNew
		initializeWith: aStore;
		yourself