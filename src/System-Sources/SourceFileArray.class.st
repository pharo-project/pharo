"
This is a variation on StandardSourceFileArray that provides a larger maximum changes file size.

The available address space for source pointers in a traditional CompiledMethod is 16r1000000 through 16r4FFFFFF. StandardSourceFileArray maps positions in the sources file to address range 16r1000000 through 16r1FFFFFF and 16r3000000 through 16r3FFFFFF, and positions in the changes file to address range 16r2000000 through 16r2FFFFFF and 16r4000000 through 16r4FFFFFF. This permits a maximum file size of 16r2000000 (32MB) for both the sources file and the changes file. 

This implementation extends the source pointer address space using bit 25 of the source pointer to identify the external sources and changes files, with the remaining high order bits treated as address extension. This limits the number of external file references to two (the traditional sources and changes files). If additional external file references are needed in the future, some higher order bits in the source pointer address space should be allocated for that purpose.

The use of bit 25 of the source pointer for file references permits backward compatibility with StandardSourceFileArray, with essentially unlimited address space expansion for the sources and changes files.

The class side instance varialbe 'sourcesFS' is an optional memory filesystem for storing sources files directly in the object memory.
"
Class {
	#name : #SourceFileArray,
	#superclass : #Object,
	#instVars : [
		'files',
		'readOnlyQueue',
		'flushChanges'
	],
	#classVars : [
		'SourcesFS'
	],
	#classInstVars : [
		'sourcesFS'
	],
	#category : #'System-Sources'
}

{ #category : #'as yet unclassified' }
SourceFileArray class >> initialize [

	SessionManager default registerSystemClassNamed: self name.
]

{ #category : #accessing }
SourceFileArray class >> sourcesFS [

	^ sourcesFS ifNil: [ 
		self class environment at: #MemoryStore
			ifPresent: [ FileSystem memory ]
			ifAbsent: [ nil ] ].
]

{ #category : #'as yet unclassified' }
SourceFileArray class >> startUp: resuming [

	resuming ifTrue: [ Smalltalk openSourceFiles ]
]

{ #category : #'public - string reading' }
SourceFileArray >> changeRecordsFor: aMethodDefinition [

	^ self 
		changeRecordsFrom: aMethodDefinition sourcePointer 
		className: aMethodDefinition methodClass theNonMetaClass name
		isMeta: aMethodDefinition methodClass isMeta
]

{ #category : #'public - string reading' }
SourceFileArray >> changeRecordsFor: aMethodDefinition detect: aUnaryBlock [
	"Try to detect the most recent ChangeRecord that satisfies aUnaryBlock. Answer nil if none satisfies."

	self
		changeRecordsFor: aMethodDefinition
		do: [ :changeRecord | 
			(aUnaryBlock value: changeRecord category) 
				ifTrue: [ ^ changeRecord ] ].
	^ nil
]

{ #category : #'public - string reading' }
SourceFileArray >> changeRecordsFor: aMethodDefinition do: aUnaryBlock [
	"Evaluate aUnaryBlock with each of the ChangeRecords of aMethodDefinition. Most recent changes are evaluated first."

	^ self 
		changeRecordsFrom: aMethodDefinition sourcePointer
		className: aMethodDefinition theNonMetaClassName
		isMeta: aMethodDefinition isMetaSide
		do: aUnaryBlock
]

{ #category : #'public - string reading' }
SourceFileArray >> changeRecordsFrom: initialSourcePointer className: theNonMetaClassName isMeta: classIsMeta [
	"Answer the ChangeRecords of a method or class comment, starting from the initialSourcePointer. 
	Most recent changes go first."
	
	| changeRecords |
	changeRecords := OrderedCollection new.

	self
		changeRecordsFrom: initialSourcePointer
		className: theNonMetaClassName
		isMeta: classIsMeta
		do: [ :changeRecord | changeRecords add: changeRecord ].
	
	^ changeRecords
]

{ #category : #'public - string reading' }
SourceFileArray >> changeRecordsFrom: initialSourcePointer className: theNonMetaClassName isMeta: classIsMeta do: aUnaryBlock [
	"Evaluate aUnaryBlock with each of the ChangeRecords of a method or class comment, starting from the initialSourcePointer. 
	Most recent changes are evaluated first."

	| filePosition fileIndex |
	fileIndex := self fileIndexFromSourcePointer: initialSourcePointer.
	filePosition := self filePositionFromSourcePointer: initialSourcePointer.	

	self readOnlyDo: [ :sourceFilesCopy | 
		| file previousFilePosition previousFileIndex preamble stamp protocol preambleTokens |
		file := sourceFilesCopy fileAt: fileIndex ifAbsent: [ ^ self ].
		
		[ filePosition notNil & file notNil ]
			whileTrue: [ 
				file position: (0 max: filePosition - 150).	"Skip back to before the preamble"
				[ file position < (filePosition - 1) ] whileTrue: [ preamble := file nextPreamble ].	"then pick it up from the front"	
					"Preamble is likely a linked method preamble, if we're in
					a changes file (not the sources file).  Try to parse it
					for prior source position and file index"

				previousFilePosition := nil.
				stamp := ''.
				"method records"
				(preamble includesSubstring: 'methodsFor:')
					ifTrue: [ preambleTokens := preamble parseLiterals ]
					ifFalse: [ preambleTokens := Array new	"ie cant be back ref" ].
				((preambleTokens size between: 7 and: 8) and: [ (preambleTokens at: preambleTokens size - 5) = #methodsFor: ])
					ifTrue: [ 
						(preambleTokens at: preambleTokens size - 3) = #stamp:
							ifTrue: [ 
								"New format gives change stamp and unified prior pointer"
								stamp := preambleTokens at: preambleTokens size - 2.
								previousFilePosition := preambleTokens last.
								previousFileIndex := self fileIndexFromSourcePointer: previousFilePosition.
								previousFilePosition := self filePositionFromSourcePointer: previousFilePosition ]
							ifFalse: [ 
								"Old format gives no stamp; prior pointer in two parts"
								previousFilePosition := preambleTokens at: preambleTokens size - 2.
								previousFileIndex := preambleTokens last ].
						(previousFilePosition = 0 or: [ previousFileIndex = 0 ])
							ifTrue: [ previousFilePosition := nil ] ].

				((preambleTokens size between: 5 and: 6) and: [ (preambleTokens at: preambleTokens size - 3) = #methodsFor: ])
					ifTrue: [ 
						(preambleTokens at: preambleTokens size - 1) = #stamp:
							ifTrue: [ 
								"New format gives change stamp and unified prior pointer"
								stamp := preambleTokens at: preambleTokens size ] ].
				"class comment records"
				(preamble includesSubstring: 'commentStamp:')
					ifTrue: [ preambleTokens := preamble parseLiterals ].
				((preambleTokens size = 5 or: [ preambleTokens size = 3 ]) and: [ (preambleTokens at: 2) = #commentStamp: ])
					ifTrue: [ 
						stamp := preambleTokens at: 3.
						preambleTokens size > 3
							ifTrue: [ 
								previousFilePosition := preambleTokens last.
								previousFileIndex := self fileIndexFromSourcePointer: previousFilePosition.
								previousFilePosition := self filePositionFromSourcePointer: previousFilePosition ].
						aUnaryBlock
							value:
								(ChangeRecord new
									file: file
									position: filePosition
									type: #classComment
									class: theNonMetaClassName
									category: nil
									meta: classIsMeta
									stamp: stamp) ]
					ifFalse: [ 
						protocol := preambleTokens after: #methodsFor: ifAbsent: [ Protocol unclassified ].
						aUnaryBlock
							value:
								(ChangeRecord new
									file: file
									position: filePosition
									type: #method
									class: theNonMetaClassName
									category: protocol
									meta: classIsMeta
									stamp: stamp) ].
									
				filePosition := previousFilePosition.
				previousFilePosition ifNotNil: [ 
					file := sourceFilesCopy 
						fileAt: previousFileIndex
						ifAbsent: [ ^ self ] ] ] ]
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
	changesFile isReadOnly ifTrue: [ ^ self ].
	changesFile setToEnd.
	
	^ aBlock value: changesFile
]

{ #category : #'public - file system operations' }
SourceFileArray >> close [
		
	| oldFiles oldQueue | 
	
	oldFiles := files.
	oldQueue := readOnlyQueue.
	files := Array new: 2.
	readOnlyQueue := SharedQueue new.

	self closeFileArray: oldFiles.

	oldQueue flushAllSuchThat: [ :each | self closeFileArray: each. true]
]

{ #category : #private }
SourceFileArray >> closeFileArray: anArray [

	anArray 
		reject: [ :file | file isNil ] 
		thenDo: [ :file | file close ]
		
]

{ #category : #private }
SourceFileArray >> createReadOnlyFiles [

	| readOnly failed |
	
	failed := false.
	readOnly := files collect: [ :file | 
		file 
			ifNil: [  failed := true. nil ]
			ifNotNil: [ [ file readOnlyCopy ] 
        		on: FileDoesNotExistException
        		do: [ failed := true. nil ] ]
	].

	^ failed ifFalse: [ readOnly ] ifTrue: [ self closeFileArray: readOnly. nil ]
]

{ #category : #'public - string writing' }
SourceFileArray >> deferFlushDuring: aBlock [

	"defer flusing the filestream until a block has been executed"
	
	flushChanges ifFalse: [ ^ aBlock value ].
	
	flushChanges := false.
	
	^ [ aBlock value ] ensure: [ flushChanges := true . self flushChangesFile ]
]

{ #category : #'public - file system operations' }
SourceFileArray >> ensureOpen [
	"Ensure that the source and changes files are opened."

	files := Array
		with: PharoFilesOpener default sourcesFileOrNil
		with: PharoFilesOpener default changesFileOrNil.
			
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
	
	self changesFileStream ifNotNil: [:f | f flush ]
]

{ #category : #'public - file system operations' }
SourceFileArray >> forceChangesToDisk [
	"Ensure that the changes file has been fully written to disk by closing and re-opening it. This makes the system more robust in the face of a power failure or hard-reboot."

	| changesFile |
	changesFile := self changesFileStream.
	changesFile flush.
	changesFile close.
	changesFile open: changesFile name forWrite: true.
	changesFile setToEnd.
]

{ #category : #'public - string reading' }
SourceFileArray >> getPreambleFrom: aFileStream at: position [
	"To read preamble of method we need read back characters until $!.
	But given aFileStream can have UTF8 encoding which make it a bit tricky to scan stream in reverse order.
	First we need to move back in stream using byte reading without encoding. It performed by basicNext.
	Next we still could not read exact number of characters because (position-startIndex) is not characters number in case of UTF8.
	So at the end we use another loop to read encoded characters step by step until stream will be at original position"
	| startIndex |
	startIndex := position.
	[startIndex >= 0 and: [aFileStream position: startIndex.
	aFileStream basicNext ~~ $!] ] whileTrue: [ startIndex := startIndex - 1 ].

	^String streamContents: [ :result | 
		[aFileStream position <= position] whileTrue: [result nextPut: aFileStream next]]

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
		sourcesFileStream: (self sourcesFileStream ifNotNil: [:aStream | aStream readOnlyCopy]);
		changesFileStream: (self changesFileStream ifNotNil: [:aStream | aStream readOnlyCopy]);
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
 
   | queue stream rofa result |
       
  	queue := readOnlyQueue.
  	rofa := queue nextOrNil ifNil: [ self createReadOnlyFiles ].
       
   rofa ifNil: [ ^ absentBlock value ].  

   stream := rofa at: index.

   position > (files at: index) size ifTrue: [ 
		self finishedReading: rofa from: queue. 
		^ absentBlock value 
	].

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

	^ RemoteString newString: aString
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
		ifPresent: [ :stream | stream nextChunk ]
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
	(file isNil or: [ file isReadOnly ]) ifTrue: [ ^ failBlock value ].

	ChangesLog default assureStartupStampLogged.
	
	file setToEnd.
	file nextPutAll: preamble.
	position := file position.
	file nextChunkPut: aString.

	self flushChangesFile.

	successBlock value: (self sourcePointerFromFileIndex: 2 andPosition: position)
]
