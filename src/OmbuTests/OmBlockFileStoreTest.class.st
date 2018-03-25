Class {
	#name : #OmBlockFileStoreTest,
	#superclass : #OmFileStoreTest,
	#category : #OmbuTests
}

{ #category : #initialization }
OmBlockFileStoreTest >> setUpOtherStore [

	^ OmBlockFileStore
		named: 'testingOtherStore', UUID new asString36
		inDirectory: self directoryForTests ensureCreateDirectory
]

{ #category : #initialization }
OmBlockFileStoreTest >> setUpStore [	

	^ OmBlockFileStore
		named: 'testingStore', UUID new asString36
		inDirectory: self directoryForTests ensureCreateDirectory
]

{ #category : #tests }
OmBlockFileStoreTest >> testMultibyteUnicodeCharacterCrossingTwoBlocks [
	"Test fogbugz case 20112"
	
	| aWideString |
	aWideString := (String streamContents: [:stream |
		store blockSize timesRepeat: [ stream nextPut: (Character value: 266) ].
	 ]).
	store newEntry: (OmEntry content: aWideString).	
	store newEntry: (OmEntry content: aWideString).	

	"self shouldnt: [ "store flush" ] raise: OmFileStoreReadingError"
]
