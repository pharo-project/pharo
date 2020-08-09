"
ZipNewFileMember instances are used to represent files that have been read from a ZipArchive.
Their data stays in the file on disk, so the original Zip file cannot be directly overwritten.
"
Class {
	#name : #ZipFileMember,
	#superclass : #ZipArchiveMember,
	#instVars : [
		'externalFileName',
		'stream',
		'localHeaderRelativeOffset',
		'dataOffset'
	],
	#category : #'Compression-Archives'
}

{ #category : #'instance creation' }
ZipFileMember class >> newFrom: stream named: fileName [
	^(self new) stream: stream externalFileName: fileName
]

{ #category : #'private-reading' }
ZipFileMember >> canonicalizeFileName [
	"For security reasons, make all paths relative and remove any ../ portions"

	[fileName beginsWith: '/'] whileTrue: [fileName := fileName allButFirst].
	fileName := fileName copyReplaceAll: '../' with: ''
]

{ #category : #initialization }
ZipFileMember >> close [
	stream ifNotNil:[stream close].
]

{ #category : #'private-writing' }
ZipFileMember >> copyDataTo: aStream [

	self copyRawDataTo: aStream.
]

{ #category : #initialization }
ZipFileMember >> initialize [
	super initialize.
	crc32 := 0.
	localHeaderRelativeOffset := 0.
	dataOffset := 0.
]

{ #category : #'private-writing' }
ZipFileMember >> localHeaderRelativeOffset [
	^localHeaderRelativeOffset
]

{ #category : #testing }
ZipFileMember >> looksLikeDirectory [
	^fileName last = $/
		and: [ uncompressedSize = 0 ]
]

{ #category : #'private-reading' }
ZipFileMember >> readCentralDirectoryFileHeaderFrom: aStream [
	"Assumes aStream positioned after signature"

	| fileNameLength extraFieldLength fileCommentLength endianStream |

	endianStream := ZnEndianessReadWriteStream on: aStream.
	self versionMadeBy: (endianStream nextLittleEndianNumber: 1).
	fileAttributeFormat := endianStream nextLittleEndianNumber: 1.

	self versionNeededToExtract: (endianStream nextLittleEndianNumber: 2).
	self bitFlag: (endianStream nextLittleEndianNumber: 2).
	compressionMethod := endianStream nextLittleEndianNumber: 2.
	lastModFileDateTime := self unzipTimestamp: (endianStream nextLittleEndianNumber: 4).
	crc32 := endianStream nextLittleEndianNumber: 4.
	compressedSize := endianStream nextLittleEndianNumber: 4.
	uncompressedSize := endianStream nextLittleEndianNumber: 4.

	fileNameLength := endianStream nextLittleEndianNumber: 2.
	extraFieldLength := endianStream nextLittleEndianNumber: 2.
	fileCommentLength := endianStream nextLittleEndianNumber: 2.
	endianStream nextLittleEndianNumber: 2. 	"disk number start"
	internalFileAttributes := endianStream nextLittleEndianNumber: 2.

	externalFileAttributes := endianStream nextLittleEndianNumber: 4.
	localHeaderRelativeOffset := endianStream nextLittleEndianNumber: 4.

	fileName := (aStream next: fileNameLength) asString.
	cdExtraField := (aStream next: extraFieldLength) asByteArray asString.
	fileComment := (aStream next: fileCommentLength) asByteArray utf8Decoded.

	self desiredCompressionMethod: compressionMethod
]

{ #category : #'private-reading' }
ZipFileMember >> readFrom: aStream [ 
	"assumes aStream positioned after CD header; leaves stream positioned after my CD entry"

	self readCentralDirectoryFileHeaderFrom: aStream.
	self readLocalDirectoryFileHeaderFrom: aStream.
	self endRead.
	self canonicalizeFileName.

]

{ #category : #'private-reading' }
ZipFileMember >> readLocalDirectoryFileHeaderFrom: aStream [ 
	"Positions stream as necessary. Will return stream to its original position"

	| fileNameLength extraFieldLength xcrc32 xcompressedSize xuncompressedSize sig oldPos endianStream |

	oldPos := aStream position.

	aStream position: localHeaderRelativeOffset.

	sig := aStream next: 4.
	sig = LocalFileHeaderSignature asByteArray
		ifFalse: [ aStream position: oldPos.
				^self error: 'bad LH signature at ', localHeaderRelativeOffset printStringHex ].

	endianStream := ZnEndianessReadWriteStream on: aStream.
	versionNeededToExtract := endianStream nextLittleEndianNumber: 2.
	self bitFlag: (endianStream nextLittleEndianNumber: 2).
	compressionMethod := endianStream nextLittleEndianNumber: 2.
	lastModFileDateTime := self unzipTimestamp: (endianStream nextLittleEndianNumber: 4).
	xcrc32 := endianStream nextLittleEndianNumber: 4.
	xcompressedSize := endianStream nextLittleEndianNumber: 4.
	xuncompressedSize := endianStream nextLittleEndianNumber: 4.

	fileNameLength := endianStream nextLittleEndianNumber: 2.
	extraFieldLength := endianStream nextLittleEndianNumber: 2.

	fileName := (aStream next: fileNameLength) asString.
	localExtraField := (aStream next: extraFieldLength) asByteArray.

	dataOffset := aStream position.

	"Don't trash these fields if we already got them from the central directory"
	self hasDataDescriptor ifFalse: [
		crc32 := xcrc32.
		compressedSize := xcompressedSize.
		uncompressedSize := xuncompressedSize.
	].

	aStream position: oldPos.
]

{ #category : #'private-reading' }
ZipFileMember >> readRawChunk: n [
	^stream next: n
]

{ #category : #'private-reading' }
ZipFileMember >> rewindData [
	super rewindData.
	(stream isNil or: [ stream closed ])
		ifTrue: [ self error: 'stream missing or closed' ].
	stream position: (localHeaderRelativeOffset + 4).
	self skipLocalDirectoryFileHeaderFrom: stream.
]

{ #category : #'private-reading' }
ZipFileMember >> skipLocalDirectoryFileHeaderFrom: aStream [ 
	"Assumes that stream is positioned after signature."

	|  extraFieldLength fileNameLength endianStream |
	aStream next: 22.
	
	endianStream := ZnEndianessReadWriteStream on: aStream.
	fileNameLength := endianStream nextLittleEndianNumber: 2.
	extraFieldLength := endianStream nextLittleEndianNumber: 2.
	aStream next: fileNameLength.
	aStream next: extraFieldLength.
	dataOffset := aStream position.

]

{ #category : #initialization }
ZipFileMember >> stream: aStream externalFileName: aFileName [
	stream := aStream.
	externalFileName := aFileName.
]

{ #category : #'private-writing' }
ZipFileMember >> uncompressDataTo: aStream [

	| decoder buffer crcErrorMessage |
	decoder := ZipReadStream on: stream.
	decoder expectedCrc: self crc32.
	buffer := ByteArray new: (32768 min: readDataRemaining).
	crcErrorMessage := nil.

	[[ readDataRemaining > 0 ] whileTrue: [
		| chunkSize |
		chunkSize := 32768 min: readDataRemaining.
		buffer := decoder next: chunkSize into: buffer startingAt: 1.
		aStream next: (chunkSize min: buffer size) putAll: buffer startingAt: 1.
		readDataRemaining := readDataRemaining - chunkSize.
	]] on: CRCError do: [ :ex | crcErrorMessage := ex messageText. ex resume ].

	crcErrorMessage ifNotNil: [ self isCorrupt: true. CRCError signal: crcErrorMessage ]


]

{ #category : #'private-writing' }
ZipFileMember >> uncompressDataTo: aStream from: start to: finish [

	| decoder buffer chunkSize |
	decoder := FastInflateStream on: stream.
	readDataRemaining := readDataRemaining min: finish - start + 1.
	buffer := ByteArray new: (32768 min: readDataRemaining).
	decoder next: start - 1.

	[ readDataRemaining > 0 ] whileTrue: [
		chunkSize := 32768 min: readDataRemaining.
		buffer := decoder next: chunkSize into: buffer startingAt: 1.
		aStream next: chunkSize putAll: buffer startingAt: 1.
		readDataRemaining := readDataRemaining - chunkSize.
	].

]

{ #category : #'private-reading' }
ZipFileMember >> unzipTimestamp: dosTimestampInteger [

	^ [ DateAndTime fromDosTimestamp: dosTimestampInteger ]
		on: Error
		do: [ "Assume we're dealing with the old, incorrect Squeak format"
			DateAndTime fromSeconds: 2492992800 + dosTimestampInteger ]
]

{ #category : #testing }
ZipFileMember >> usesFile: aFileReferenceOrFileName [
	"Do I require aFileName? That is, do I care if it's clobbered?"
	^ externalFileName asFileReference = aFileReferenceOrFileName asFileReference
]
