"
This is a variation on StandardSourceFileArray that provides a larger maximum changes file size.

The available address space for source pointers in a traditional CompiledMethod is 16r1000000 through 16r4FFFFFF. StandardSourceFileArray maps positions in the sources file to address range 16r1000000 through 16r1FFFFFF and 16r3000000 through 16r3FFFFFF, and positions in the changes file to address range 16r2000000 through 16r2FFFFFF and 16r4000000 through 16r4FFFFFF. This permits a maximum file size of 16r2000000 (32MB) for both the sources file and the changes file. 

This implementation extends the source pointer address space using bit 25 of the source pointer to identify the external sources and changes files, with the remaining high order bits treated as address extension. This limits the number of external file references to two (the traditional sources and changes files). If additional external file references are needed in the future, some higher order bits in the source pointer address space should be allocated for that purpose.

The use of bit 25 of the source pointer for file references permits backward compatibility with StandardSourceFileArray, with essentially unlimited address space expansion for the sources and changes files.

"
Class {
	#name : #SourceFileArray,
	#superclass : #Object,
	#instVars : [
		'files',
		'readOnlyQueue',
		'flushChanges'
	],
	#category : #'System-Sources-Sources'
}

{ #category : #'class initialization' }
SourceFileArray class >> initialize [

	SessionManager default registerSystemClassNamed: self name.
]

{ #category : #'class initialization' }
SourceFileArray class >> startUp: resuming [

	resuming ifTrue: [ Smalltalk openSourceFiles ]
]

{ #category : #accessing }
SourceFileArray >> changesFileStream [
	"Answer the master .changes FileStream, with writing permissions."

	^ files at: 2
]

{ #category : #accessing }
SourceFileArray >> changesFileStream: aStream [

	^ files at: 2 put: aStream
]

{ #category : #'public - string writing' }
SourceFileArray >> changesWriteStreamDo: aBlock [

	| changesFile |
	changesFile := self changesFileStream.
	changesFile ifNil: [ ^ self ].
	changesFile closed ifTrue: [ ^ self ].
	changesFile isReadOnly ifTrue: [ ^ self ].
	changesFile setToEnd.
	
	^ aBlock value: changesFile
]

{ #category : #'public - file system operations' }
SourceFileArray >> close [
		
	| oldFiles |
	oldFiles := files.
	files := Array new: 2.
	self closeFileArray: oldFiles.
	self emptyReadStreamsQueue
]

{ #category : #private }
SourceFileArray >> closeFileArray: anArray [

	anArray 
		reject: [ :file | file isNil ] 
		thenDo: [ :file | file close ].
		
	^ anArray
]

{ #category : #private }
SourceFileArray >> createReadOnlyFiles [
	| readOnly |
	readOnly := {
		self sourcesFileStream ifNotNil: [self sourcesFileStream readOnlyCopy].
		self sourcesFileStream ifNotNil: [self changesFileStream readOnlyCopy] }.
	^ (readOnly anySatisfy: [ :any | any isNil or: [any closed] ])
			ifTrue: [ self closeFileArray: readOnly ]
			ifFalse: [ readOnly ]
]

{ #category : #'public - string writing' }
SourceFileArray >> deferFlushDuring: aBlock [

	"defer flusing the filestream until a block has been executed"
	
	flushChanges ifFalse: [ ^ aBlock value ].
	
	flushChanges := false.
	
	^ [ aBlock value ] ensure: [
		flushChanges := true.
		self flushChangesFile ]
]

{ #category : #'public - file system operations' }
SourceFileArray >> emptyReadStreamsQueue [
		
	| oldQueue | 
	oldQueue := readOnlyQueue.
	readOnlyQueue := SharedQueue new.
	oldQueue flushAllSuchThat: [ :each | self closeFileArray: each. true]
]

{ #category : #'public - file system operations' }
SourceFileArray >> ensureOpen [
	"Ensure that the source and changes files are opened.
	Close them before re-opening them."
	
	| aSourcesFile aChangesFile |

	self close.

	aSourcesFile := PharoFilesOpener default sourcesFileOrNil.
	aChangesFile := PharoFilesOpener default changesFileOrNil.
	
	aChangesFile ifNil: [ 
		aChangesFile := PharoFilesOpener default changesFileOrNilReadOnly: true silent: false ].

	files := Array
		with: aSourcesFile
		with: aChangesFile.
			
	readOnlyQueue := SharedQueue new.
]

{ #category : #'private - accessing file streams' }
SourceFileArray >> fileAt: index [
	^ files at: index
]

{ #category : #'private - accessing file streams' }
SourceFileArray >> fileAt: index ifAbsent: aBlock [
	^ files at: index ifAbsent: aBlock
]

{ #category : #'private - sourcepointer conversion' }
SourceFileArray >> fileIndexFromSourcePointer: anInteger [
	"Return the index of the source file which contains the source chunk addressed by anInteger"

	(anInteger bitAnd: 16r1000000) ~= 0
		ifTrue: [^1 "sources file"]
		ifFalse: [anInteger >= 16r1000000
			ifTrue: [^2 "changes file"]
			ifFalse: [^0 "compatibility with StandardSourceFileArray"]]
]

{ #category : #'private - sourcepointer conversion' }
SourceFileArray >> filePositionFromSourcePointer: anInteger [ 
	"Return the position of the source chunk addressed by anInteger"

	| hi lo |
	hi := anInteger // 33554432.
	lo := anInteger \\ 16777216.
	((anInteger bitAnd: 16777216) ~= 0
			or: [anInteger < 16777216 "compatibility with StandardSourceFileArray"])
		ifTrue: [^ hi * 16777216 + lo"sources file"]
		ifFalse: [^ hi - 1 * 16777216 + lo"changes file"]
]

{ #category : #private }
SourceFileArray >> finishedReading: aReadOnlyFileArray from: aQueue [

	(aQueue == readOnlyQueue and: [ aQueue size < self readOnlyQueueLimit ]) 
		ifTrue: [ aQueue nextPut: aReadOnlyFileArray ]
		ifFalse: [ self closeFileArray: aReadOnlyFileArray ]
]

{ #category : #'public - file system operations' }
SourceFileArray >> flushChangesFile [
	
	flushChanges ifFalse: [ ^ self ].
	self changesFileStream ifNotNil: [:stream | stream flush].
]

{ #category : #'public - file system operations' }
SourceFileArray >> forceChangesToDisk [
	"Ensure that the changes file has been fully written to disk by closing and re-opening it. This makes the system more robust in the face of a power failure or hard-reboot."

	| changesFile |
	changesFile := self changesFileStream.
	changesFile isReadOnly ifFalse: [ 
		changesFile flush ].
	changesFile close.
	changesFile tryOpen.
	changesFile setToEnd
]

{ #category : #'public - string reading' }
SourceFileArray >> getPreambleFrom: aSourceFileStream at: position [
	^ aSourceFileStream getPreambleAt: position 
]

{ #category : #initialization }
SourceFileArray >> initialize [

	files := Array new: 2.
	
	readOnlyQueue := SharedQueue new.
	flushChanges := true
]

{ #category : #'public - string reading' }
SourceFileArray >> protocolAt: sourcePointer [
	"Answer the protocol for a given method, retrieved from the sources or changes file. Answer unfound protocol if no protocol is available"
				
	^ self protocolAt: sourcePointer for: self sourceDataPointers
]

{ #category : #'public - string reading' }
SourceFileArray >> protocolAt: sourcePointer for: sourceDataPointers [
	"Answer the protocol for a given method, retrieved from the sources or changes file. Answer unfound protocol if no protocol is available."

	| preamble protocol tokens protocolPosition |
	protocol := 'unfound protocol'.	"this is to indicate that the tagging in the source does not use the correct format.
	We will have to fix that. For example some traits methods are wrongly tagged.
	see http://code.google.com/p/pharo/issues/detail?id=4581"
	preamble := self sourcedDataAt: sourcePointer.
	preamble = 'Trait method' ifTrue: [ ^ nil ].
	(preamble includesSubstring: sourceDataPointers key) ifTrue: [ 
		tokens := preamble parseLiterals.
		protocolPosition := tokens indexOf: sourceDataPointers key.
		protocolPosition = 0
			ifFalse: [ 
				"New format gives change protocol and unified prior pointer"
				protocol := tokens at: protocolPosition + 1 ] ].
	^ protocol
]

{ #category : #'private - copying' }
SourceFileArray >> readOnlyCopy [
	"Answer a read only copy of self. The caller is responsible of closing it."

	^ self species new
		sourcesFileStream: self sourcesFileStream readOnlyCopy;
		changesFileStream: self changesFileStream readOnlyCopy;
		yourself
]

{ #category : #'private - copying' }
SourceFileArray >> readOnlyDo: aBlock [
	"Value a block with a read only copy of self."

	| copy |
	copy := self readOnlyCopy.

	^ [ aBlock value: copy ] ensure: [ copy close ]
]

{ #category : #private }
SourceFileArray >> readOnlyQueueLimit [

	"maximum number of read only file arrays to be held by the readOnlyQueue(SharedQueue)"

	^ 7
]

{ #category : #'private - accessing file streams' }
SourceFileArray >> readStreamAt: sourcePointer ifPresent: presentBlock ifAbsent: absentBlock [

	| index position |
	sourcePointer ifNil: [ ^ absentBlock value ].
	
	index := self fileIndexFromSourcePointer: sourcePointer.
	index = 0 ifTrue: [ ^ absentBlock value ].	"no source pointer for this element"

	position := self filePositionFromSourcePointer: sourcePointer.
	position = 0 ifTrue: [ ^ absentBlock value ].

	^ self 
		readStreamAtFileIndex: index 
		atPosition: position 
		ifPresent: presentBlock 
		ifAbsent: absentBlock
]

{ #category : #'private - accessing file streams' }
SourceFileArray >> readStreamAtFileIndex: index atPosition: position ifPresent: presentBlock ifAbsent: absentBlock [
 
   | queue stream rofa result file |
   
	"This method is really ugly. 
	It uses different readers over the same file that is beeing writing. 
	This can lead to buffer problems. 
	This should be rethougth to a better way.
	
	Maybe an unification of all the accesses using the same buffer could be nice."
    
  	queue := readOnlyQueue.
  	rofa := queue nextOrNil ifNil: [ self createReadOnlyFiles ].
       
   rofa ifNil: [ ^ absentBlock value ].  

   stream := rofa at: index.
	stream ifNil: [ ^ absentBlock value ].
	
	file := files at: index.
	
   position > file size ifTrue: [ 
		self finishedReading: rofa from: queue. 
		^ absentBlock value 
	].

	"If the file has been flushed, I am discarding the readonly stream's buffer.
	This is to avoid a problem where the readOnly buffer is missing data that was in 
	the writer buffer."	
	file ensureWrittenPosition: position.

	"I am discarding the buffer of the readonly stream, because if not is out-of-sync."
	stream discardBuffer.
	
   stream position: position.
   result := presentBlock value: stream.
       
	self finishedReading: rofa from: queue. 
                      
    ^ result

]

{ #category : #'public - string reading' }
SourceFileArray >> remoteStringAt: sourcePointer [

	| filePosition fileIndex |
	fileIndex := self fileIndexFromSourcePointer: sourcePointer.
	filePosition := self filePositionFromSourcePointer: sourcePointer.	

	^ RemoteString newFileNumber: fileIndex position: filePosition
]

{ #category : #'public - string writing' }
SourceFileArray >> remoteStringForNewString: aString [

	^ RemoteString new
		sourceFileArray: self;
		string: aString;
		yourself
]

{ #category : #printing }
SourceFileArray >> reportString [

	^ String streamContents: [ :aStream | 
		{ self sourcesFileStream. self changesFileStream } 
			do: [:each | each ifNotNil: [
				aStream nextPutAll: each printString; cr ] ] ]
]

{ #category : #'public - string reading' }
SourceFileArray >> sourceCodeAt: sourcePointer [ 
	"Retrieves the source code for the pointer received as argument"
	
	^ self
		readStreamAt: sourcePointer 
		ifPresent: [ :stream | (ChunkReadStream on: stream) next ]
		ifAbsent: [ '' ]
]

{ #category : #private }
SourceFileArray >> sourceDataPointers [
	"Retrieves the combination key to look for information of the receiver in the source file"

	"'Point methodsFor: ''point functions'' stamp: ''lr 7/4/2009 10:42'''."

	^ 'methodsFor:' -> #stamp:
]

{ #category : #'private - sourcepointer conversion' }
SourceFileArray >> sourcePointerFromFileIndex: index andPosition: position [
	"Return a sourcePointer encoding the given file index and position"

	| hi lo |
	(index = 1 or: [index = 2])
		ifFalse: [self error: 'invalid source file index'].
	position < 0 ifTrue: [self error: 'invalid source code pointer'].
	hi := position // 16r1000000 *2 + index.
	lo := position \\ 16r1000000.
	^ hi * 16r1000000 + lo

]

{ #category : #'public - string reading' }
SourceFileArray >> sourcedDataAt: sourcePointer [

	^ self 
		readStreamAt: sourcePointer 
		ifPresent: [ :file | 
			| position |
			position := self filePositionFromSourcePointer: sourcePointer.
			self getPreambleFrom: file at: (0 max: position - 3) ]
		ifAbsent: [ '' ]
]

{ #category : #accessing }
SourceFileArray >> sourcesFileStream [
	"Answer the master .sources FileStream, with writing permissions."

	^ files at: 1
]

{ #category : #accessing }
SourceFileArray >> sourcesFileStream: aStream [

	^ files at: 1 put: aStream
]

{ #category : #'public - string reading' }
SourceFileArray >> timeStampAt: sourcePointer [
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file. Answer the empty string if no time stamp is available."

	^ self timeStampAt: sourcePointer for: self sourceDataPointers
]

{ #category : #'public - string reading' }
SourceFileArray >> timeStampAt: sourcePointer for: sourceDataPointers [
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file. Answer the empty string if no time stamp is available."

	| preamble stamp tokens stampPosition |
	stamp := ''.
	flushChanges ifFalse: [ ^ stamp ].
	preamble := self sourcedDataAt: sourcePointer.
	(preamble includesSubstring: sourceDataPointers key)
		ifTrue: [ 
			tokens := preamble parseLiterals.
			stampPosition := tokens indexOf: sourceDataPointers value.
			stampPosition = 0
				ifFalse: [ 
					"New format gives change stamp and unified prior pointer"
					stamp := tokens at: stampPosition + 1 ] ].
	^ stamp
]

{ #category : #'public - string writing' }
SourceFileArray >> writeSource: aString preamble: preamble onSuccess: successBlock onFail: failBlock [

	| file position |
	file := self changesFileStream.
	(file isNil or: [file closed or: [ file isReadOnly ] ]) ifTrue: [ ^ failBlock value ].

	ChangesLog default assureStartupStampLogged.
	
	file setToEnd.
	file nextPutAll: preamble.
	position := file position.
	(SourceChunkWriteStream on: file) nextChunkPut: aString.

	successBlock cull: (self sourcePointerFromFileIndex: 2 andPosition: position) cull: file.

	self flushChangesFile.

]
