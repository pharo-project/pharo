tests
testLoadFromCorruptFile1
	"Bad character"

	| loadedStore |
	store newEntry: (OmEntry content: 41).
	store newEntry: (OmEntry content: 42).

	"Override first characters"
	store fileReference writeStreamDo: [ :stream | stream nextPutAll: '***' ].
	
	loadedStore := 
		(OmFileStore 
			named: store globalName 
			inDirectory: self directoryForTests)
			yourself.
	
	self 
		should: [ loadedStore refresh; entries ]
		raise: OmFileStoreReadingError