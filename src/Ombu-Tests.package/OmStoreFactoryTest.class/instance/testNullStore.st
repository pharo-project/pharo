tests
testNullStore

	| factory store |
	factory := OmStoreFactory new.
	store := factory null.
	self assert: store entries isEmpty.
	self assert: factory null == store description: 'Singleton'