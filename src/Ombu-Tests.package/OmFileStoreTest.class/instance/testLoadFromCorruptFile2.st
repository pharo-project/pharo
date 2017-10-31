tests
testLoadFromCorruptFile2
	"Bad class name"

	| loadedStore |
	store newEntry: (OmEntry content: 41).
	store newEntry: (OmEntry content: 42).

	"Override first characters"
	store fileReference writeStreamDo: [ :stream | stream nextPutAll: 'OMENTRY' ].
	
	loadedStore := 
		(OmFileStore 
			named: store globalName 
			inDirectory: self directoryForTests)
			yourself.
	
	self 
		should: [ loadedStore refresh; entries ]
		raise: OmFileStoreReadingError