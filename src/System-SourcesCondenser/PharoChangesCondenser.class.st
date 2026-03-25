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
		'sourceStreams',
		'condensationStamp'
	],
	#classVars : [
		'ConserveTimestamps'
	],
	#category : 'System-SourcesCondenser',
	#package : 'System-SourcesCondenser'
}

{ #category : 'helper' }
PharoChangesCondenser class >> condense [
	^ self new
		condense
]

{ #category : 'accessing' }
PharoChangesCondenser class >> conserveTimestamp [
	"It takes a lot of time to access the timestamp of all methods in the sources file, so by default we do not save a new timestamp because the Pharo Boostrap happens in a few minutes, there is no need to save at the second."

	^ ConserveTimestamps ifNil: [ ConserveTimestamps := false ]
]

{ #category : 'accessing' }
PharoChangesCondenser class >> conserveTimestamp: aBoolean [

	ConserveTimestamps := aBoolean
]

{ #category : 'private - 3 installing' }
PharoChangesCondenser >> backupOldChanges [
	| changesFile |

	changesFile := self originalFile.
	changesFile moveTo: (changesFile , 'bak') asFileReference nextVersion
]

{ #category : 'private' }
PharoChangesCondenser >> basicCondense [

	(CodeExporterV100 onStream: stream)
		writeFileoutVersion.
	self
		condenseClassesAndTraits;
		swapSourcePointers.
	stream flush
]

{ #category : 'accessing' }
PharoChangesCondenser >> condensationStamp [
	"Used if we do not conserve the timestamp of the methods in the system"

	^ condensationStamp ifNil: [ condensationStamp := DateAndTime now printString ]
]

{ #category : 'public' }
PharoChangesCondenser >> condense [

	SourcesManager current class = FilesSourcesManager ifFalse: [ self error: 'The condenser can work only in the current source manager is the files manager.' ].
	job := [
		       newChangesFile writeStreamDo: [ :aStream |
				       stream := aStream.
				       self basicCondense ].
		       self
			       installNewChangesFile;
			       reset ] asJob.

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
	"Return the index into the FilesSourcesManager:
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

	SourcesManager current changesFileStream close.
	self backupOldChanges.
	self originalFile ensureDelete.
	newChangesFile moveTo: self originalFile.
	SourcesManager current reset
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

	^ FileLocator changes asFileReference
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

	^ (SourcesManager current fileIndexFromSourcePointer: aMethod sourcePointer) == 2
]

{ #category : 'private - 1 writing' }
PharoChangesCondenser >> shouldWriteClassComment: commentRemoteString [

	^ commentRemoteString isNotNil and: [ (SourcesManager current fileIndexFromSourcePointer: commentRemoteString) ~= 1 ]
]

{ #category : 'private - 2 swapping' }
PharoChangesCondenser >> sourcePointerFor: remoteString [

	^ SourcesManager current sourcePointerFromFileIndex: remoteString first andPosition: remoteString second
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

	job currentValue: 0 title: 'Swapping source pointers'.

	Smalltalk allClassesAndTraitsDo: [ :classOrTrait |
			job increment.
			self swapSourcePointerOfClassOrTrait: classOrTrait ]
]

{ #category : 'accessing' }
PharoChangesCondenser >> temporaryFile [

	^ (FileLocator changes , 'new') nextVersion
]

{ #category : 'accessing' }
PharoChangesCondenser >> timestampForClass: aClass [
	"It takes a lot of time to access the timestamp of all methods in the sources file, so by default we do not save a new timestamp because the Pharo Boostrap happens in a few minutes, there is no need to save at the second."

	self class conserveTimestamp ifFalse: [ ^ self condensationStamp ].
	^ aClass commentStamp
]

{ #category : 'accessing' }
PharoChangesCondenser >> timestampForMethod: aMethod [
	"It takes a lot of time to access the timestamp of all methods in the sources file, so by default we do not save a new timestamp because the Pharo Boostrap happens in a few minutes, there is no need to save at the second."

	self class conserveTimestamp ifFalse: [ ^ self condensationStamp ].
	^ aMethod timeStamp
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
			stamp := (self timestampForClass: aClass).
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
				store: (self timestampForMethod: aMethod) ].

	self writeRemoteString: aMethod sourceCode for: aMethod.

	stream
		nextPutAll: ' !';
		cr
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
