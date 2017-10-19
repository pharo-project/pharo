tests
testMultibyteUnicodeCharacterCrossingTwoBlocks
	"Test fogbugz case 20112"
	
	| aWideString |
	aWideString := (String streamContents: [:stream |
		store blockSize timesRepeat: [ stream nextPut: (Character value: 266) ].
	 ]).
	store newEntry: (OmEntry content: aWideString).	
	store newEntry: (OmEntry content: aWideString).	

	self shouldnt: [ store flush ] raise: OmFileStoreReadingError