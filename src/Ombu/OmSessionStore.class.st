"
I wrap a file store that is renewed each time either the Smalltalk session changes, among others (check #needsReset).
"
Class {
	#name : #OmSessionStore,
	#superclass : #OmStore,
	#instVars : [
		'currentSession',
		'currentImagePathString',
		'currentStore',
		'baseLocator',
		'announcer',
		'writingDeferDuration',
		'headReference',
		'storeNameStrategy'
	],
	#classInstVars : [
		'storeNameStrategy',
		'defaultBaseLocator'
	],
	#category : #'Ombu-Stores'
}

{ #category : #accessing }
OmSessionStore class >> defaultBaseLocator [

	^ defaultBaseLocator ifNil: [ 
		defaultBaseLocator := FileLocator localDirectory / 'ombu-sessions' ]
]

{ #category : #accessing }
OmSessionStore class >> defaultBaseLocator: aFileLocator [ 

	defaultBaseLocator := aFileLocator
]

{ #category : #initialization }
OmSessionStore class >> initialize [
	"
	self initialize
	"
	SessionManager default registerUserClassNamed: self name

]

{ #category : #'instance creation' }
OmSessionStore class >> new [

	^ self newWithBaseLocator: self defaultBaseLocator
]

{ #category : #'instance creation' }
OmSessionStore class >> newWithBaseLocator: aBaseDirectory [

	^ self basicNew
		initializeWithBaseLocator: aBaseDirectory
		yourself
]

{ #category : #'system startup' }
OmSessionStore class >> startUp [
	"The #store accessor refreshes each instance since it will discover that the Session changed"

	self allInstancesDo: #store 
]

{ #category : #accessing }
OmSessionStore class >> storeNameStrategy [

	^ storeNameStrategy ifNil: [ 
		storeNameStrategy := OmRandomSuffixStrategy new ]
]

{ #category : #accessing }
OmSessionStore class >> storeNameStrategy: aStoreNameStrategy [

	storeNameStrategy := aStoreNameStrategy
]

{ #category : #accessing }
OmSessionStore >> announcer [

	^ announcer ifNil: [ 
		announcer := Announcer new ]
]

{ #category : #accessing }
OmSessionStore >> baseLocator [

	^ baseLocator
]

{ #category : #accessing }
OmSessionStore >> directory [

	^ baseLocator asFileReference
]

{ #category : #enumerating }
OmSessionStore >> entriesDo: aBlockClosure [
	"Evaluate aBlockClosure with every entries this store contains"

	^ self store entriesDo: aBlockClosure
]

{ #category : #accessing }
OmSessionStore >> entryFor: aReference ifPresent: presentBlockClosure ifAbsent: absentBlockClosure [
	"Look for an entry, evaluating either the first block closure if present or the second if absent."

	^ self store 
		entryFor: aReference 
		ifPresent: presentBlockClosure 
		ifAbsent: absentBlockClosure
]

{ #category : #testing }
OmSessionStore >> existsStoreNamed: aName [

	^ OmFileStore existsStoreNamed: aName inDirectory: self directory

]

{ #category : #refreshing }
OmSessionStore >> flush [
	"Flush any buffered state"
	
	self store flush
]

{ #category : #accessing }
OmSessionStore >> globalName [

	^ self store globalName
]

{ #category : #accessing }
OmSessionStore >> headReference [
	"Answer a reference to the last entry added to this store."

	^ headReference ifNil: [
		headReference := self store headReference ]
]

{ #category : #private }
OmSessionStore >> imagePathString [
	"Answer the full path name for the current image."

	^ SmalltalkImage current imagePath
]

{ #category : #initialization }
OmSessionStore >> initializeWithBaseLocator: aDirectoryFileLocator [

	self initialize.
	
	baseLocator := aDirectoryFileLocator.
	self store. "Force initialization"
	headReference := self store headReference.

]

{ #category : #accessing }
OmSessionStore >> lowLevelFileStoreIfNone: aBlock [
	"Needed by EpLostChangesDetector"
	
	^ currentStore ifNil: aBlock
]

{ #category : #private }
OmSessionStore >> needsReset [

	^ currentSession ~~ Smalltalk session
		or: [ currentImagePathString ~= self imagePathString ]
]

{ #category : #writing }
OmSessionStore >> newEntry: anEntry [
	"Persist a new entry"

	self store newEntry: anEntry.
	headReference := self store headReference.
]

{ #category : #accessing }
OmSessionStore >> nextStoreName [

	| name |
	name := currentStore 
		ifNil: [ "First store name"
			Smalltalk imageFile basenameWithoutExtension, '.1' ]
		ifNotNil: [ "There is already a name"
			currentStore globalName ].
		
	^ self storeNameStrategy nextTo: name in: self directory

]

{ #category : #refreshing }
OmSessionStore >> refresh [

	^ self store refresh
]

{ #category : #accessing }
OmSessionStore >> resetWithNextStoreName [

	self resetWithStoreNamed: self nextStoreName
]

{ #category : #accessing }
OmSessionStore >> resetWithStoreNamed: newName [

	| fileReference |
	"First, ensure file is not already used."
	fileReference := OmFileStore 
		fileReferenceForStoreNamed: newName
		inDirectory: self directory.
	fileReference exists ifTrue: [
		FileExists signalWith: fileReference ].
	
	self directory fileSystem isWritable ifFalse: [ ^ self ].
	
	"Then, we can proceed."
	currentSession := Smalltalk session.
	currentImagePathString := self imagePathString.
	currentStore := 
		OmStoreFactory current
			named: newName
			inDirectory: self directory ensureCreateDirectory.
	currentStore writingDeferDuration: self writingDeferDuration.
	
	"Announce"
	self announcer announce: OmSessionStoreUpdated new.
]

{ #category : #accessing }
OmSessionStore >> store [
	"Answer the store I wrap"

	(currentStore isNil or: [ self needsReset ]) 
		ifTrue: [ self resetWithNextStoreName ].
	
	^ currentStore
]

{ #category : #accessing }
OmSessionStore >> storeNameStrategy [

	^ storeNameStrategy ifNil: [ self class storeNameStrategy ]
]

{ #category : #accessing }
OmSessionStore >> storeNameStrategy: anOmStoreNameStrategy [

	storeNameStrategy := anOmStoreNameStrategy
]

{ #category : #accessing }
OmSessionStore >> writingDeferDuration [

	^ writingDeferDuration ifNil: [ OmFileStore defaultWritingDeferDuration ]
]

{ #category : #accessing }
OmSessionStore >> writingDeferDuration: aDuration [

	writingDeferDuration := aDuration.
	self store writingDeferDuration: aDuration.
]

{ #category : #accessing }
OmSessionStore >> writingFileReference [

	^ currentStore writingFileReference
]
