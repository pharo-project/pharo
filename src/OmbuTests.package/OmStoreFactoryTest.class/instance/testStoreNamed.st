tests
testStoreNamed

	| factory store storeAgain anEntry |
	factory := OmStoreFactory new.
	store := factory named: 'aTestGlobalName'.
	storeAgain := factory named: 'aTestGlobalName'.
	self assert: storeAgain == store description: 'Singleton'.

	anEntry := OmEntry content: 42.
	store newEntry: anEntry.
	self assert: (store entries includes: anEntry).
