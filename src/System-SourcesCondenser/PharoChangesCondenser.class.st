"
I create a new more compact changes files with a single version of each method in the image.
"
Class {
	#name : 'PharoChangesCondenser',
	#superclass : 'Object',
	#instVars : [
		'newChangesFile',
		'stream',
		'job',
		'remoteStringMap',
		'sourceStreams'
	],
	#category : 'System-SourcesCondenser',
	#package : 'System-SourcesCondenser'
}

{ #category : 'helper' }
PharoChangesCondenser class >> condense [
	^ self new
		condense
]

{ #category : 'private - 3 installing' }
PharoChangesCondenser >> backupOldChanges [
	| changesFile |

	changesFile := self originalFile.
	changesFile moveTo: (changesFile , 'bak') asFileReference nextVersion
]

{ #category : 'private' }
PharoChangesCondenser >> basicCondense [

	self
		condenseClassesAndTraits;
		swapSourcePointers.
	stream flush
]

{ #category : 'public' }
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
		run
]

{ #category : 'private' }
PharoChangesCondenser >> condenseClassOrTrait: classOrTrait [
	self writeClassComment: classOrTrait.

	classOrTrait instanceSide methodsDo: [ :method |
		(self shouldCondenseMethod: method)
			ifTrue: [ self writeMethodSource: method ]].

	classOrTrait classSide methodsDo: [ :method |
		(self shouldCondenseMethod: method)
			ifTrue: [ self writeMethodSource: method ]]
]

{ #category : 'private' }
PharoChangesCondenser >> condenseClassesAndTraits [
	Smalltalk allClassesAndTraitsDo: [ :classOrTrait |
		self condenseClassOrTrait: classOrTrait	]
]

{ #category : 'accessing' }
PharoChangesCondenser >> fileIndex [
	"Return the index into the SourceFileArray:
	1: the .sources file
	2. the .changes file"
	^ 2
]

{ #category : 'initialization' }
PharoChangesCondenser >> initialize [

	super initialize.
	self reset
]

{ #category : 'private - 3 installing' }
PharoChangesCondenser >> installNewChangesFile [

	SourceFileArray default changesFileStream close.
	self backupOldChanges.
	self originalFile ensureDelete.
	newChangesFile moveTo: self originalFile.
	Smalltalk openSourceFiles
]

{ #category : 'helper' }
PharoChangesCondenser >> nextChunkDo: aBlock [
	(ChunkWriteStream on: stream) nextPut: (String streamContents: aBlock)
]

{ #category : 'helper' }
PharoChangesCondenser >> nextCommentChunkDo: aBlock [
	stream cr; nextPut: $!.
	self nextChunkDo: aBlock.
	stream cr
]

{ #category : 'accessing' }
PharoChangesCondenser >> originalFile [
	^ Smalltalk changesFile asFileReference
]

{ #category : 'initialization' }
PharoChangesCondenser >> reset [
	remoteStringMap := IdentityDictionary new.
	newChangesFile := self temporaryFile.

	"Keep a copy of the source streams for performance"
	sourceStreams := Array
			with: PharoFilesOpener default sourcesFileOrNil
			with: PharoFilesOpener default changesFileOrNil
]

{ #category : 'private - testing' }
PharoChangesCondenser >> shouldCondenseMethod: aMethod [
	"Only write methods with changes in the current file (not .sources)"

	^ (SourceFileArray default fileIndexFromSourcePointer: aMethod sourcePointer) == 2
]

{ #category : 'private - 1 writing' }
PharoChangesCondenser >> shouldWriteClassComment: commentRemoteString [

	^ commentRemoteString notNil
		and: [ (SourceFileArray default fileIndexFromSourcePointer: commentRemoteString) ~= 1 ]
]

{ #category : 'private - 2 swapping' }
PharoChangesCondenser >> sourcePointerFor: remoteString [

	^ SourceFileArray default sourcePointerFromFileIndex: remoteString first andPosition: remoteString second
]

{ #category : 'private - 2 swapping' }
PharoChangesCondenser >> swapClassComment: classOrTrait [

	remoteStringMap at: classOrTrait ifPresent: [ :remoteString |
			| sourcePointer |
			sourcePointer := self sourcePointerFor: remoteString.
			classOrTrait commentSourcePointer: sourcePointer ]
]

{ #category : 'private - 2 swapping' }
PharoChangesCondenser >> swapSourcePointerOfClassOrTrait: classOrTrait [

	self swapClassComment: classOrTrait.

	classOrTrait methodsDo: [ :method |
		self swapSourcePointerOfMethod: method ].

	classOrTrait classSide methodsDo: [ :method |
		self swapSourcePointerOfMethod: method ]
]

{ #category : 'private - 2 swapping' }
PharoChangesCondenser >> swapSourcePointerOfMethod: method [

	remoteStringMap at: method ifPresent: [ :remoteString |
			| sourcePointer |
			sourcePointer := self sourcePointerFor: remoteString.
			method setSourcePointer: sourcePointer ]
]

{ #category : 'private - 2 swapping' }
PharoChangesCondenser >> swapSourcePointers [
	job
		title: 'Swapping source pointers';
		currentValue: 0.

	Smalltalk allClassesAndTraitsDo: [ :classOrTrait |
		job increment.
		self swapSourcePointerOfClassOrTrait: classOrTrait ]
]

{ #category : 'accessing' }
PharoChangesCondenser >> temporaryFile [
	^ (Smalltalk changesFile, 'new') nextVersion
]

{ #category : 'private - 1 writing' }
PharoChangesCondenser >> writeClassComment: aClass [

	| commentSourcePointer stamp |
	commentSourcePointer := aClass commentSourcePointer.

	(self shouldWriteClassComment: commentSourcePointer) ifFalse: [ ^ self ].

	self nextCommentChunkDo: [ :strm |
			strm
				nextPutAll: aClass name;
				nextPutAll: ' commentStamp: '.
			stamp := aClass commentStamp ifNil: [ '<historical>' ].
			stamp storeOn: strm ].

	self writeRemoteString: aClass comment for: aClass
]

{ #category : 'private - 1 writing' }
PharoChangesCondenser >> writeMethodSource: aMethod [

	self nextCommentChunkDo: [ :strm |
		strm
			nextPutAll: aMethod methodClass name;
			nextPutAll: ' methodsFor: ';
			store: aMethod protocolName asString;
			nextPutAll: ' stamp: ';
			store: aMethod timeStamp ].

	self
		writeRemoteString: aMethod sourceCode
		for: aMethod.

	stream nextPutAll: ' !'; cr
]

{ #category : 'private - 1 writing' }
PharoChangesCondenser >> writeRemoteString: aString for: owner [

	| remoteString position |
	
	position := stream position.
	remoteString := {self fileIndex .  position}.
	(ChunkWriteStream on: stream) nextPut: aString.
	remoteStringMap at: owner put: remoteString.

	^ remoteString
]
