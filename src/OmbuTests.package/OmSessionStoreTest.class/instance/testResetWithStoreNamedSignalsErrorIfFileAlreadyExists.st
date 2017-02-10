tests
testResetWithStoreNamedSignalsErrorIfFileAlreadyExists
	
	store resetWithStoreNamed: 'pepe'.
	store newEntry: (OmEntry content: 42).
	store flush.
	
	self 
		should: [ store resetWithStoreNamed: 'pepe' ]
		raise: FileExists