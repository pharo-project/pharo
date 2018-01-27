"
SUnit tests for MultiByteFileStream 

"
Class {
	#name : #MultiByteFileStreamTest,
	#superclass : #TestCase,
	#category : #Deprecated70
}

{ #category : #support }
MultiByteFileStreamTest >> chunkString [
  "A chunk larger than buffer (currently 2048), but not long enough to fill it completely again. Therefore, buffer should contain some elements at end that are not part of the real buffer"
  | smallChunk chunkString |
  chunkString := 
  String new: (MultiByteFileStream new ascii enableReadBuffering braceArray size * 1.3) floor.
  smallChunk := 'ASDFASDFASDFASDFasdfasdfasdfasdfQWERQWERQWERqwerqwerqwer! !'.
  1 to: chunkString size by: smallChunk size do: [:ix |
    chunkString replaceFrom: ix to: (ix + smallChunk size -1 min: chunkString size) with: smallChunk startingAt: 1].
  ^chunkString
]

{ #category : #accessing }
MultiByteFileStreamTest >> fileOutTestClass [
	^ MultiByteFileStreamFileOutTestClass 
]

{ #category : #support }
MultiByteFileStreamTest >> lineEndTestFile [
	^'lineEndTesting.txt'
]

{ #category : #running }
MultiByteFileStreamTest >> tearDown [
	'foobug6933' asFileReference ensureDelete.
	self lineEndTestFile asFileReference ensureDelete.
	super tearDown 
]

{ #category : #testing }
MultiByteFileStreamTest >> testBasicChunk [
	 | internalStream chunkKey |
	internalStream := ChunkReadStream on: self chunkString readStream.
	chunkKey := OrderedCollection new.
	[internalStream atEnd] whileFalse: 
		[ | chunk |
		chunk := internalStream next.
		chunkKey add: {internalStream position. chunk size. chunk}].
	[self writeChunkToFile.
	(File named: 'chunkTest.txt') readStreamDo: [:fileStream | | stream |
		stream := ChunkReadStream on: (ZnCharacterReadStream on: fileStream).
		1 to: chunkKey size do: [:ix | |chunk|
			chunk := stream next.
			self assert: (chunkKey at: ix) second equals: chunk size.
			self assert: (chunkKey at: ix) last equals: chunk.]].
	] ensure: [ | f |
		f := File named: 'chunkTest.txt'.
		f exists ifTrue: [ f delete ]]
]

{ #category : #testing }
MultiByteFileStreamTest >> testBinaryUpTo [
	"This is a non regression test for bug http://bugs.squeak.org/view.php?id=6933"
	"self run: #testBinaryUpTo"
	
	| foo fileName |
	
	fileName := 'foobug6933'. 
	MultiByteFileStream 
		forceNewFileNamed: fileName 
		do: [ :stream | stream binary.
				stream nextPutAll: #[1 2 3 4] ].

	foo := MultiByteFileStream oldFileNamed: fileName.
	[foo binary.
		self assert: (foo upTo: 3) = #[1 2] 
	] ensure: [
		foo close.
 		fileName  asFileReference delete]. 
    
]

{ #category : #testing }
MultiByteFileStreamTest >> testFileOutNonAscii [
	"My test class, class side, contains a method with non-ascii characters, 
	fileOut used to fail due to #backChunk problems"
	
	(self fileOutTestClass name asString, '.st') asFileReference ensureDelete.
	self fileOutTestClass fileOut
]

{ #category : #testing }
MultiByteFileStreamTest >> testLineEndingsWith: anEncodingName [
	 | byteMap |
	"Make a map of different endings, and the expected byte pattern when three cr's are added using it"
	byteMap := Dictionary new.
	byteMap at: #cr put: #[13 13 13];
			at: #lf put: #[10 10 10];
			at: #crlf put: #[13 10 13 10 13 10].
	byteMap keysDo: [:lineEnding  |
		FileStream forceNewFileNamed: self lineEndTestFile do: [:stream | 
			stream 
				lineEndConvention: lineEnding;
				converter: (TextConverter newForEncoding: anEncodingName).
			"Test 3 cases: Use of nextPut, nextPutAll:, and nextPutAll with WideStrings"
			stream nextPut: Character cr;
					nextPutAll: Character cr asString;
					nextPutAll: Character cr asString asWideString.].
		FileStream oldFileNamed: self lineEndTestFile do: [:stream | | bytes expected| 
			bytes := stream binary; contents.
			self assert: bytes equals: (byteMap at: lineEnding)]]
]

{ #category : #testing }
MultiByteFileStreamTest >> testNext [
	| testString filename  |
	testString := 'éèàôüößäóñíá'.
	filename := 'test-file-' , 99 atRandom printString , '.txt'.
	MultiByteFileStream 
		forceNewFileNamed: filename
		do: [ :stream | 
			stream
				nextPutAll: testString;
				crlf ].
	[  
	filename asFileReference
		readStreamDo: [ :stream |
			self assert: (stream peek = $é ).
			self assert: (stream next = $é ).
			self assert: (stream next = $è ).
		] ]
	ensure: [ filename asFileReference ensureDelete ] 
]

{ #category : #testing }
MultiByteFileStreamTest >> testNextMatchAll [
	| testString filename  |
	testString := 'é è à ô ü ö ß ä ó ñ í á'.
	filename := 'test-file-' , 99 atRandom printString , '.txt'.
	MultiByteFileStream 
		forceNewFileNamed: filename
		do: [ :stream | 
			stream
				nextPutAll: testString;
				crlf ].
	[  
	filename asFileReference
		readStreamDo: [ :stream |
			self assert: (stream nextMatchAll:  testString).
		] ]
	ensure: [ filename asFileReference ensureDelete ] 
]

{ #category : #testing }
MultiByteFileStreamTest >> testNextPut [
	| filename  |
	filename := 'test-file-' , 99 atRandom printString , '.txt'.
	MultiByteFileStream 
		forceNewFileNamed: filename
		do: [ :stream | 
			stream
				nextPut: $1;
				nextPut: $a;
				nextPut: $2;
				nextPut: $b ;
				crlf].
	[  
	filename asFileReference
		readStreamDo: [ :stream |
			self assert: (stream nextMatchAll:  '1a2b').
		] ]
	ensure: [ filename asFileReference ensureDelete ] 
]

{ #category : #testing }
MultiByteFileStreamTest >> testNextPutAll [
	| testString filename  |
	testString := '123abcd'.
	filename := 'test-file-' , 99 atRandom printString , '.txt'.
	MultiByteFileStream 
		forceNewFileNamed: filename
		do: [ :stream | 
			stream
				nextPutAll: '123';
				nextPutAll: 'abcd';
				crlf].
	[  
	filename asFileReference
		readStreamDo: [ :stream |
			self assert: (stream nextMatchAll:  testString).
		] ]
	ensure: [ filename asFileReference ensureDelete ] 
]

{ #category : #testing }
MultiByteFileStreamTest >> testNextPutAllStartingAt [
	| testString filename  |
	testString := 'éèàôüößäóñíá'.
	filename := 'test-file-' , 99 atRandom printString , '.txt'.
	MultiByteFileStream 
		forceNewFileNamed: filename
		do: [ :stream | 
			stream
				next: 3 putAll: testString startingAt: 3;
				crlf].
	[  
	filename asFileReference
		readStreamDo: [ :stream |
			self assert: (stream nextMatchAll: 'àôü' ).
		] ]
	ensure: [ filename asFileReference ensureDelete ] 
]

{ #category : #testing }
MultiByteFileStreamTest >> testPeek [
	| testString filename  |
	testString := 'é è à ô ü ö ß ä ó ñ í á'.
	filename := 'test-file-' , 99 atRandom printString , '.txt'.
	MultiByteFileStream 
		forceNewFileNamed: filename
		do: [ :stream | 
			stream
				nextPutAll: testString;
				crlf ].
	[  
	filename asFileReference
		readStreamDo: [ :stream |
			self assert: (stream peek = $é ).
			self assert: (stream next = $é ).
			stream upToEnd.
			self assert: (stream peek = nil ).
		] ]
	ensure: [ filename asFileReference ensureDelete ] 
]

{ #category : #testing }
MultiByteFileStreamTest >> testPeekFor [
	| testString filename  |
	testString := 'éèàôüößäóñíá'.
	filename := 'test-file-' , 99 atRandom printString , '.txt'.
	MultiByteFileStream 
		forceNewFileNamed: filename
		do: [ :stream | 
			stream
				nextPutAll: testString;
				crlf ].
	[  
	filename asFileReference
		readStreamDo: [ :stream |
			self assert: (stream peekFor: $é ).
			self assert: (stream peekFor: $è ).
			self deny: (stream peekFor: $a). "If peekFor fails, it doesn't advance position"
			self assert: (stream peekFor: $à)
		] ]
	ensure: [ filename asFileReference ensureDelete ] 
]

{ #category : #testing }
MultiByteFileStreamTest >> testReadIntoStartingAtCount [
	| testString filename buffer |
	testString := 'élève en Français'.
	filename := 'test-file-' , 99 atRandom printString , '.txt'.
	filename asFileReference ensureDelete.
	filename asFileReference
		writeStreamDo: [ :stream | 
			stream
				nextPutAll: testString;
				crlf ].
	buffer := String new: testString size.
	[ 
	filename asFileReference
		readStreamDo: [ :stream | 
			self assert: (stream readInto: buffer startingAt: 1 count: testString size) equals: testString size.
			self assert: buffer equals: testString ] ]
		ensure: [ filename asFileReference ensureDelete ]
]

{ #category : #testing }
MultiByteFileStreamTest >> testReset [
	| testString filename  |
	testString := 'éèàôüößäóñíá'.
	filename := 'test-file-' , 99 atRandom printString , '.txt'.
	MultiByteFileStream 
		forceNewFileNamed: filename
		do: [ :stream | 
			stream
				nextPutAll: testString;
				crlf ].
	[  
	filename asFileReference
		readStreamDo: [ :stream |
			self assert: (stream peekFor: $é ).
			self assert: (stream peekFor: $è ).
			stream reset.
			self assert: (stream peekFor: $é ).
		] ]
	ensure: [ filename asFileReference ensureDelete ] 
]

{ #category : #testing }
MultiByteFileStreamTest >> testUpToAnyOfDo [
	| testString filename  str1 ch1 ch2|
	testString := 'élève en Français'.
	filename := 'test-file-' , 99 atRandom printString , '.txt'.
	MultiByteFileStream 
		forceNewFileNamed: filename
		do: [ :stream | 
			stream
				nextPutAll: testString. ].
	
	filename asFileReference
		readStreamDo: [ :stream | 
			str1 := stream upToAnyOf: 'd e n F' do: [ :c | ch1 := c].
			self assert: (ch1 = $e).
			self assert: (str1 = 'élèv' ).
			str1 := stream upToAnyOf: 'xyz' do: [ :c | ch2 := c ].
			self assert: (str1 = ' en Français').
			self assert: (ch2 isNil ).  
		] 
]

{ #category : #testing }
MultiByteFileStreamTest >> testUpToEnd [
	| testString filename  |
	testString := 'élève en Français'.
	filename := 'test-file-' , 99 atRandom printString , '.txt'.
	MultiByteFileStream 
		forceNewFileNamed: filename
		do: [ :stream | 
			stream
				nextPutAll: testString .
		].
	
	filename asFileReference
		readStreamDo: [ :stream | 
			| str1 |
			self assert: ( stream peek = $é ).
			self assert: ( (stream next: 5) = 'élève' ).
			str1 := stream upToEnd .
			self assert: ( str1 = ' en Français') .
		] 
]

{ #category : #support }
MultiByteFileStreamTest >> writeChunkToFile [
	(File openAndTruncateForWriteFileNamed: 'chunkTest.txt') 
		nextPutAll: self chunkString;
		close
]
