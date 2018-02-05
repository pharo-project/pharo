Class {
	#name : #OmSessionStoreTest,
	#superclass : #OmStoreTest,
	#category : #OmbuTests
}

{ #category : #resources }
OmSessionStoreTest >> directoryForTests [

	^ FileSystem workingDirectory
]

{ #category : #resources }
OmSessionStoreTest >> setUpOtherStore [

	^ OmSessionStore newWithBaseLocator: (self directoryForTests / 'test-sessions-other') ensureCreateDirectory
]

{ #category : #resources }
OmSessionStoreTest >> setUpStore [	

	^ OmSessionStore newWithBaseLocator: (self directoryForTests / 'test-sessions-main') ensureCreateDirectory
]

{ #category : #running }
OmSessionStoreTest >> tearDown [
		
	super tearDown. "needs to be executed before"

	(self directoryForTests / 'test-sessions-main') ensureDeleteAll.
	(self directoryForTests / 'test-sessions-other') ensureDeleteAll
]

{ #category : #tests }
OmSessionStoreTest >> testResetWithNextStoreNameWithRandomSuffix [

	| aFileReference anotherFileReference |
	store storeNameStrategy: OmRandomSuffixStrategy new.

	store resetWithNextStoreName.
	aFileReference := store writingFileReference.
	store resetWithNextStoreName.
	anotherFileReference := store writingFileReference.
	
	self deny: aFileReference = anotherFileReference
]

{ #category : #tests }
OmSessionStoreTest >> testResetWithNextStoreNameWithSequentialSuffix [
	"The sequencial suffix doesn't change unless the file is created"
	
	| aFileReference anotherFileReference |
	store storeNameStrategy: OmSequentialSuffixStrategy new.
	
	store resetWithNextStoreName.
	aFileReference := store writingFileReference.
	store resetWithNextStoreName.
	anotherFileReference := store writingFileReference.
	
	self assert: aFileReference equals: anotherFileReference.
	
	store newEntry: (OmEntry content: 42).
	store flush.
	store resetWithNextStoreName.
	anotherFileReference := store writingFileReference.
	self deny: aFileReference = anotherFileReference.
]

{ #category : #tests }
OmSessionStoreTest >> testResetWithNextStoreNameWithTimeStampSuffix [

	| aFileReference anotherFileReference |
	store storeNameStrategy: OmTimeStampSuffixStrategy new.

	store resetWithNextStoreName.
	aFileReference := store writingFileReference.
	50 milliSeconds wait.
	store resetWithNextStoreName.
	anotherFileReference := store writingFileReference.
	
	self deny: aFileReference = anotherFileReference
]

{ #category : #tests }
OmSessionStoreTest >> testResetWithStoreNamedDoesAnnounce [

	| wasAnnounced |
	wasAnnounced := false.

	store announcer
		subscribe: OmSessionStoreUpdated
		do: [ :announcement | wasAnnounced := true ].
	
	[ store resetWithStoreNamed: 'new' ] 
		ensure: [ store announcer unsubscribe: self ].

	self assert: wasAnnounced
	
]

{ #category : #tests }
OmSessionStoreTest >> testResetWithStoreNamedSignalsErrorIfFileAlreadyExists [
	
	store resetWithStoreNamed: 'pepe'.
	store newEntry: (OmEntry content: 42).
	store flush.
	
	self 
		should: [ store resetWithStoreNamed: 'pepe' ]
		raise: FileExists
]
