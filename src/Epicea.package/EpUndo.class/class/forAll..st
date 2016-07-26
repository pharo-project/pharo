instance creation
forAll: aCollectionOfEntryReferences
	^ self basicNew
		initializeWith: aCollectionOfEntryReferences;
		yourself