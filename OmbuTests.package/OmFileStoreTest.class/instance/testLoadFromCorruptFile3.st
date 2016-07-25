tests
testLoadFromCorruptFile3
	"Bad class name. Last entry."

	| loadedStore |
	store newEntry: (OmEntry content: 41).

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