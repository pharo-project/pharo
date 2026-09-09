Class {
	#name : 'SourceFile',
	#superclass : 'Object',
	#instVars : [
		'stream',
		'potentialLocations',
		'formatVersionClass',
		'fileName'
	],
	#category : 'System-Sources-Files',
	#package : 'System-Sources-Files'
}

{ #category : 'instance creation' }
SourceFile class >> createFileNamed: aFileReference version: aVersion [ 
	
	| file |
	file := self lookupFileNamed: aFileReference basename potentialLocations: { aFileReference parent }.
	file version: aVersion.
	file forceCreateSourceFile.
	^ file
]

{ #category : 'instance creation' }
SourceFile class >> lookupFileNamed: aString potentialLocations: locations [

	^ self new
		  fileName: aString;
		  potentialLocations: locations;
		  yourself
]

{ #category : 'testing' }
SourceFile >> atEnd [

	^ stream atEnd
]

{ #category : 'initialization' }
SourceFile >> close [

	stream ifNil: [ ^ self ].
	stream closed ifTrue: [
			stream := nil.
			^ self ].
	stream close.
	stream := nil
]

{ #category : 'accessing' }
SourceFile >> closed [
	^ stream isNil or: [ stream closed ]
]

{ #category : 'stop conditions' }
SourceFile >> cr [

	stream cr
]

{ #category : 'versions' }
SourceFile >> declareVersion [
	
	| newStream |
	newStream := self newSourceStreamReadOnly: false.	
	newStream ifNil: [ Error signal: 'Cannot create source file: cannot open file' ].
	newStream size = 0 ifFalse: [ Error signal: 'Cannot create source file: file already exists' ].	
	
	(self exporterClass onStream: newStream) writeFileoutVersion.
	newStream flush.
	stream := newStream.
]

{ #category : 'versions' }
SourceFile >> detectParserVersion [
	
	"Let's find a compatible parser for this file"
	{ChunkFileFormatParserV200 . ChunkFileFormatParserV100} do: [ :p |
		| parser |
		parser := p for: stream.
		[ ^ parser parseVersion ]
			on: CodeImportError do: [ :e | 
				"Ignore only errors related to version parsing.
				And try with the next parser if that is the case".
				stream close. self tryOpen ] ].
	
	self error: 'Something went really wrong here!'
]

{ #category : 'buffering' }
SourceFile >> discardBuffer [

	stream wrappedStream discardBuffer
]

{ #category : 'opening' }
SourceFile >> ensureSourceFileCreated [

	"Make sure that the source file is set up if it does not exist"
	(self newSourceStreamReadOnly: true) ifNotNil: [ ^ self ].
	
	"If the file does not exist, open it for write and declare the default version v1.0.0"
	self version: ChunkFileFormatParserV100.
	self forceCreateSourceFile
]

{ #category : 'accessing' }
SourceFile >> ensureWrittenPosition: aPosition [

	self isReadOnly ifTrue: [ ^ false ].

	^ stream ensureWrittenPosition: aPosition
]

{ #category : 'versions' }
SourceFile >> exporterClass [

	^ (formatVersionClass ifNil: [ formatVersionClass := self formatVersion ]) writer
]

{ #category : 'accessing' }
SourceFile >> fileName [

	^ fileName
]

{ #category : 'accessing' }
SourceFile >> fileName: aString [

	fileName := aString
]

{ #category : 'accessing' }
SourceFile >> flush [

	(stream isNil or: [ stream isReadOnly ])
		ifTrue: [ ^ self ].

	stream flush
]

{ #category : 'opening' }
SourceFile >> forceCreateSourceFile [
	
	"If the file does not exist, open it for write and declare the default version v1.0.0"
	self declareVersion.
	self close
]

{ #category : 'formatting' }
SourceFile >> formatVersion [
	
	self tryOpen.
	stream ifNil: [ self error ].
	
	^ self detectParserVersion
]

{ #category : 'accessing' }
SourceFile >> fullName [

	^ self fileName asString
]

{ #category : 'accessing' }
SourceFile >> getPreambleAt: startingPosition [
	"Search backwards from byte startingPosition in my stream for a method preamble and return it.
	A method preamble looks like: MyClass methodsFor: 'test' stamp: 'author 1/27/2019 12:27'
	but with exclamation marks ($!) around it (the contents excluding the $!'s is returned).
	startingPosition should be set one position before the closing $!"

	| characterReadStream binaryStream encoder position |
	"I hold either a ZnCharacterReadStream or a ZnCharacterReadWriteStream (see #tryOpenReadOnly:)
	Use #isReadOnly and #readOnlyCopy to access the ZnCharacterReadStream in both cases"
	characterReadStream := self isReadOnly ifTrue: [ stream ] ifFalse: [ stream readOnlyCopy ].
	"Access the binary read stream wrapped by the character read stream"
	binaryStream := characterReadStream wrappedStream.
	"Access the encoder held by the character read stream"
	encoder := characterReadStream encoder.
	"Search backwards for the previous occurrence of $!
	Although the underlying encoding is UTF-8 we can still operate/move at the byte level
	since $! code 33 cannot occur in code points encoded using 2, 3 or 4 bytes"
	position := startingPosition.
	[ position >= 0
		and: [
			binaryStream position: position.
			binaryStream next ~= 33 "$!" ] ]
		whileTrue: [ position := position - 1 ].
	"Now that we found the byte range, extract and decode it"
	^ encoder decodeBytes: (binaryStream next: startingPosition - position)
]

{ #category : 'testing' }
SourceFile >> isOpen [

	^ stream isNotNil and: [ stream closed not ]
]

{ #category : 'testing' }
SourceFile >> isReadOnly [

	^ stream isReadOnly
]

{ #category : 'streaming' }
SourceFile >> newSourceStreamReadOnly: readOnly [

	| newStream |
	
	readOnly ifFalse: [
			potentialLocations do: [ :each |
					[
						newStream := SourceFileCharacterReadWriteStream
							             on: (SourceFileBufferedReadWriteStream on: (each asFileReference / self fileName) unbufferedBinaryWriteStream)
							             encoding: 'utf8'.
						^ newStream ]
						on: Error
						do: [ ] ] ].
	
	"Open a read write stream only if read only access was not requested.
	We need to create the encoding and buffering streams manually because we need a read write stream."
	potentialLocations do: [ :each |
			  [
				  newStream := ZnCharacterReadStream on: (each asFileReference / self fileName) binaryReadStream encoding: 'utf8'.
				  ^ newStream ]
				  on: Error
				  do: [ ] ].

	^ nil
]

{ #category : 'instance creation' }
SourceFile >> newWriter [

	^ SourceFileWriter new
		  sourceFile: self;
		  yourself
]

{ #category : 'accessing' }
SourceFile >> next [

	^ stream next
]

{ #category : 'accessing' }
SourceFile >> next: anInteger [

	^ stream next: anInteger
]

{ #category : 'accessing' }
SourceFile >> next: anInteger putAll: aString startingAt: startIndex [

	stream next: anInteger putAll: aString startingAt: startIndex
]

{ #category : 'streaming' }
SourceFile >> nextChunk [

	^ (ChunkReadStream on: stream) next
]

{ #category : 'accessing' }
SourceFile >> nextPut: aCharacter [

	stream nextPut: aCharacter
]

{ #category : 'accessing' }
SourceFile >> nextPutAll: aString [

	stream nextPutAll: aString
]

{ #category : 'accessing' }
SourceFile >> peek [

	^ stream peek
]

{ #category : 'accessing' }
SourceFile >> position [

	^ stream position
]

{ #category : 'accessing' }
SourceFile >> position: anInteger [

	stream position: anInteger
]

{ #category : 'accessing' }
SourceFile >> potentialLocations [

	^ potentialLocations
]

{ #category : 'accessing' }
SourceFile >> potentialLocations: aCollection [

	potentialLocations := aCollection
]

{ #category : 'printing' }
SourceFile >> printOn: aStream [

	aStream
		nextPutAll: self class name;
		nextPut: $(;
		nextPutAll: self fullName;
		nextPut: $)
]

{ #category : 'accessing' }
SourceFile >> readOnlyCopy [

	^ (self species lookupFileNamed: self fileName potentialLocations: potentialLocations)
		  tryOpenReadOnly: true;
		  yourself
]

{ #category : 'initialization' }
SourceFile >> setToEnd [

	stream setToEnd
]

{ #category : 'accessing' }
SourceFile >> size [

	^ stream size
]

{ #category : 'accessing' }
SourceFile >> skip: anInteger [

	stream skip: anInteger
]

{ #category : 'streaming' }
SourceFile >> space [

	stream space
]

{ #category : 'accessing' }
SourceFile >> stream: aStream [

	stream := aStream
]

{ #category : 'opening' }
SourceFile >> tryOpen [

	self tryOpenReadOnly: false
]

{ #category : 'opening' }
SourceFile >> tryOpenReadOnly: readOnly [

	| newStream |
	"Open a read write stream only if read only access was not requested.
	We need to create the encoding and buffering streams manually because we need a read write stream."
	newStream := self newSourceStreamReadOnly: readOnly.
	
	newStream ifNotNil: [
		"The file exists and we have the right to at least read it"
		newStream size = 0 ifTrue: [
			"If empty, then the file has not been open as a source file"
			Error signal: 'Cannot open non source file: ', self fullName asString
		] ].
	
	stream := newStream.
]

{ #category : 'accessing' }
SourceFile >> upToEnd [

	^ stream upToEnd
]

{ #category : 'versions' }
SourceFile >> version: aClass [
	
	formatVersionClass := aClass
]

{ #category : 'stop conditions' }
SourceFile >> writeChangesDuring: aBlock onSuccess: successBlock onFail: failBlock [

	| sourceCodePosition |
	(self closed or: [ self isReadOnly ]) ifTrue: [ ^ failBlock value ].
	self setToEnd.

	sourceCodePosition := aBlock value: self.
	successBlock value: sourceCodePosition
]
