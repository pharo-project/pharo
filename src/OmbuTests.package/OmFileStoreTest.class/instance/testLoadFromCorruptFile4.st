tests
testLoadFromCorruptFile4
	"Content is corrupted"

	| loadedStore |
	store newEntry: (OmEntry content: 41).
	store newEntry: (OmEntry content: 42).
	store flush.
	
	"Override first characters"
	store fileReference writeStreamDo: [ :stream | 
		stream 
			upToAll: '41';
			skip: -1;
			nextPutAll: '**' ].
	
	loadedStore := 
		(OmFileStore 
			named: store globalName 
			inDirectory: self directoryForTests)
			yourself.
	
	self 
		should: [ loadedStore refresh; entries ]
		raise: OmFileStoreReadingError