"
I create a new more compact changes files with a single version of each method in the image.
"
Class {
	#name : #PharoChangesCondenser,
	#superclass : #Object,
	#instVars : [
		'newChangesFile',
		'stream',
		'job',
		'remoteStringMap',
		'sourceStreams'
	],
	#category : #'System-SourcesCondenser'
}

{ #category : #helper }
PharoChangesCondenser class >> condense [
	^ self new
		condense
]

{ #category : #'private - 3 installing' }
PharoChangesCondenser >> backupOldChanges [
	| changesFile |
	
	changesFile := self originalFile.
	changesFile moveTo: (changesFile , 'bak') asFileReference nextVersion.
	
]

{ #category : #private }
PharoChangesCondenser >> basicCondense [
	self
		condenseClassesAndTraits;
		swapSourcePointers;
    updateQuitPosition.
  stream flush.
]

{ #category : #public }
PharoChangesCondenser >> condense [
	job := [ 
		newChangesFile writeStreamDo: [ :aStream |
			  stream := aStream.
			  self basicCondense ].
  		self
        installNewChangesFile;
  		  reset
    ] asJob.
	
	job 
		title: 'Condensing Changes';
		max: Smalltalk classNames size + Smalltalk traitNames size;
		run.
]

{ #category : #private }
PharoChangesCondenser >> condenseClassOrTrait: classOrTrait [
	self writeClassComment: classOrTrait.
	
	classOrTrait instanceSide methodsDo: [ :method |
		(self shouldCondenseMethod: method)
			ifTrue: [ self writeMethodSource: method ]].
	
	classOrTrait classSide methodsDo: [ :method |
		(self shouldCondenseMethod: method)
			ifTrue: [ self writeMethodSource: method ]]
]

{ #category : #private }
PharoChangesCondenser >> condenseClassesAndTraits [ 
	Smalltalk allClassesAndTraitsDo: [ :classOrTrait | 
		self condenseClassOrTrait: classOrTrait	]
	
]

{ #category : #accessing }
PharoChangesCondenser >> fileIndex [
	"Return the index into the SourceFiles:
	1: the .sources file
	2. the .changes file"
	^ 2
]

{ #category : #initialization }
PharoChangesCondenser >> initialize [
	self reset.
]

{ #category : #'private - 3 installing' }
PharoChangesCondenser >> installNewChangesFile [
	
	SourceFiles changesFileStream close.
	self backupOldChanges.
	self originalFile ensureDelete.
	newChangesFile moveTo: self originalFile.
	Smalltalk openSourceFiles.
]

{ #category : #helper }
PharoChangesCondenser >> nextChunkDo: aBlock [
	(ChunkWriteStream on: stream) nextPut: (String streamContents: aBlock)
]

{ #category : #helper }
PharoChangesCondenser >> nextCommentChunkDo: aBlock [
	stream cr; nextPut: $!.
	self nextChunkDo: aBlock.
	stream cr
]

{ #category : #accessing }
PharoChangesCondenser >> originalFile [
	^ Smalltalk changesFile asFileReference
]

{ #category : #initialization }
PharoChangesCondenser >> reset [
	remoteStringMap := IdentityDictionary new.
	newChangesFile := self temporaryFile.
	
	"Keep a copy of the source streams for performance"
	sourceStreams := Array
			with: PharoFilesOpener default sourcesFileOrNil
			with: PharoFilesOpener default changesFileOrNil
]

{ #category : #'private - testing' }
PharoChangesCondenser >> shouldCondenseMethod: aMethod [
	"Only write methods with changes in the current file (not .sources)"

	^ (SourceFiles fileIndexFromSourcePointer: aMethod sourcePointer) == 2
]

{ #category : #'private - 2 swapping' }
PharoChangesCondenser >> swapClassComment: classOrTrait [
	remoteStringMap 
		at: classOrTrait
		ifPresent: [ :remoteString | 
			classOrTrait organization comment: remoteString ]
]

{ #category : #'private - 2 swapping' }
PharoChangesCondenser >> swapSourcePointerOfClassOrTrait: classOrTrait [

	self swapClassComment: classOrTrait.
	
	classOrTrait methodsDo: [ :method | 
		self swapSourcePointerOfMethod: method ].
	
	classOrTrait classSide methodsDo: [ :method | 
		self swapSourcePointerOfMethod: method ]
]

{ #category : #'private - 2 swapping' }
PharoChangesCondenser >> swapSourcePointerOfMethod: method [

	remoteStringMap at: method ifPresent: [ :remoteString |
		method setSourcePointer: remoteString sourcePointer ]
]

{ #category : #'private - 2 swapping' }
PharoChangesCondenser >> swapSourcePointers [
	job 
		title: 'Swapping source pointers';
		currentValue: 0.
	
	Smalltalk allClassesAndTraitsDo: [ :classOrTrait |
		job increment.
		self swapSourcePointerOfClassOrTrait: classOrTrait ]
	
]

{ #category : #accessing }
PharoChangesCondenser >> temporaryFile [
	^ (Smalltalk changesFile, 'new') nextVersion.
]

{ #category : #'private - 3 installing' }
PharoChangesCondenser >> updateQuitPosition [
	
	Smalltalk lastQuitLogPosition: stream position.
]

{ #category : #'private - 1 writing' }
PharoChangesCondenser >> writeClassComment: aClass [
	| organizer commentRemoteString stamp |
	
	organizer := aClass organization.
	commentRemoteString := organizer commentRemoteString.
	
	(commentRemoteString isNil or: [ commentRemoteString sourceFileNumber = 1 ])
		ifTrue: [ ^ self ].
		
	self nextCommentChunkDo: [ :strm |
		strm 
			nextPutAll: aClass name;
			nextPutAll: ' commentStamp: '.
		stamp := organizer commentStamp 
			ifNil: ['<historical>'].
		stamp storeOn: strm ].
	
	self 
		writeRemoteString: organizer classComment
		for: aClass
]

{ #category : #'private - 1 writing' }
PharoChangesCondenser >> writeMethodSource: aMethod [
	
	self nextCommentChunkDo: [ :strm |
		strm 
			nextPutAll: aMethod methodClass name;
			nextPutAll: ' methodsFor: ';
			store: aMethod category asString;
			nextPutAll: ' stamp: ';
			store: aMethod timeStamp ].
	
	self
		writeRemoteString: aMethod sourceCode
		for: aMethod.
		
	stream nextPutAll: ' !'; cr.
]

{ #category : #'private - 1 writing' }
PharoChangesCondenser >> writeRemoteString: aString for: reference [

	| remoteString |
	remoteString := 
		RemoteString 
			newString: aString 
			onFileNumber: self fileIndex 
			toFile: stream.

	remoteStringMap at: reference put: remoteString.

	^ remoteString
]
