tests
testStoreNamed

	| factory store storeAgain anEntry |
	factory := OmStoreFactory new.
	store := factory named: self aStoreName.
	storeAgain := factory named: self aStoreName.
	self assert: storeAgain == store description: 'Singleton'.

	anEntry := OmEntry content: 42.
	store newEntry: anEntry.
	self assert: (store entries includes: anEntry).

	"tearDown in test"
	store flush.
	store ensureDeleteFile.
