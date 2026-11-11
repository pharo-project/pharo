"
ZipNewFileMember instances are used to represent files that have been read from a ZipArchive.
Their data stays in the file on disk, so the original Zip file cannot be directly overwritten.
"
Class {
	#name : 'ZipFileMember',
	#superclass : 'ZipArchiveMember',
	#instVars : [
		'externalFileName',
		'stream',
		'localHeaderRelativeOffset',
		'dataOffset'
	],
	#category : 'Compression-Archives',
	#package : 'Compression',
	#tag : 'Archives'
}

{ #category : 'instance creation' }
ZipFileMember class >> newFrom: stream named: fileName [
	^(self new) stream: stream externalFileName: fileName
]

{ #category : 'private - reading' }
ZipFileMember >> canonicalizeFileName [
	"For security reasons (zip slip), make all paths relative: remove leading slashes, a leading drive letter and any '.' or '..' segment.
	Backslashes are treated as separators too, since some Windows tools write them."

	| isDirectory segments |
	isDirectory := fileName notEmpty and: [ '/\' includes: fileName last ].
	segments := (fileName copyReplaceAll: '\' with: '/') findTokens: '/'.
	segments := segments reject: [ :each | each = '.' or: [ each = '..' ] ].
	(segments notEmpty and: [ self isDriveLetter: segments first ]) ifTrue: [ segments := segments allButFirst ].
	fileName := '/' join: segments.
	(isDirectory and: [ fileName notEmpty ]) ifTrue: [ fileName := fileName , '/' ]
]

{ #category : 'initialization' }
ZipFileMember >> close [
	stream ifNotNil:[stream close]
]

{ #category : 'private - writing' }
ZipFileMember >> copyDataTo: aStream [

	compressionMethod = CompressionStored ifFalse: [ ^ self copyRawDataTo: aStream ].
	self copyStoredDataCheckingCrcTo: aStream
]

{ #category : 'private - writing' }
ZipFileMember >> copyStoredDataCheckingCrcTo: aStream [
	"Copy my stored (not compressed) data to aStream and check it against the CRC-32 read from the archive headers, as #uncompressDataTo: does for deflated data."

	self checkCrc: (self copyRawDataComputingCrcTo: aStream)
]

{ #category : 'accessing' }
ZipFileMember >> externalFile: aFileReference [
	externalFileName := aFileReference
]

{ #category : 'testing' }
ZipFileMember >> hasEncryptedData [
	"Answer whether my data in the archive is encrypted. The flag is read from
	my local header, so that it reflects the data and not a flag changed since."

	| oldPosition flag |
	(stream isNil or: [ stream closed ]) ifTrue: [ ^ self isEncrypted ].
	oldPosition := stream position.
	stream position: localHeaderRelativeOffset + 6.
	flag := stream next.
	stream position: oldPosition.
	^ (flag bitAnd: EncryptedFlag) ~= 0
]

{ #category : 'initialization' }
ZipFileMember >> initialize [
	super initialize.
	crc32 := 0.
	localHeaderRelativeOffset := 0.
	dataOffset := 0
]

{ #category : 'private - reading' }
ZipFileMember >> isDriveLetter: aString [
	"Answer whether aString is a Windows drive such as 'C:'"

	^ aString size = 2 and: [ aString first isLetter and: [ aString last = $: ] ]
]

{ #category : 'private - writing' }
ZipFileMember >> localHeaderRelativeOffset [
	^localHeaderRelativeOffset
]

{ #category : 'testing' }
ZipFileMember >> looksLikeDirectory [

	^ (fileName notEmpty and: [ fileName last = $/ ]) and: [ uncompressedSize = 0 ]
]

{ #category : 'private - reading' }
ZipFileMember >> nextZip64ValueFrom: aStream [

	aStream atEnd ifTrue: [ ZipArchiveError signal: 'missing Zip64 extended information for ' , fileName printString ].
	^ aStream next
]

{ #category : 'private - reading' }
ZipFileMember >> readCentralDirectoryFileHeaderFrom: aStream [
	"Assumes aStream positioned after signature"

	| fileNameLength extraFieldLength fileCommentLength endianStream fileNameBytes |
	endianStream := ZnEndianessReadWriteStream on: aStream.
	self versionMadeBy: (endianStream nextLittleEndianNumber: 1).
	fileAttributeFormat := endianStream nextLittleEndianNumber: 1.

	self readCommonHeaderFieldsFrom: endianStream.
	crc32 := endianStream nextLittleEndianNumber: 4.
	compressedSize := endianStream nextLittleEndianNumber: 4.
	uncompressedSize := endianStream nextLittleEndianNumber: 4.

	fileNameLength := endianStream nextLittleEndianNumber: 2.
	extraFieldLength := endianStream nextLittleEndianNumber: 2.
	fileCommentLength := endianStream nextLittleEndianNumber: 2.
	endianStream nextLittleEndianNumber: 2. "disk number start"
	internalFileAttributes := endianStream nextLittleEndianNumber: 2.

	externalFileAttributes := endianStream nextLittleEndianNumber: 4.
	localHeaderRelativeOffset := endianStream nextLittleEndianNumber: 4.

	fileNameBytes := aStream next: fileNameLength.
	cdExtraField := (aStream next: extraFieldLength) asByteArray asString.
	fileName := self fileNameFrom: fileNameBytes extraField: cdExtraField.
	fileComment := self decodeText: (aStream next: fileCommentLength).
	self readZip64ValuesFromCentralDirectory.

	self desiredCompressionMethod: compressionMethod
]

{ #category : 'private - reading' }
ZipFileMember >> readCommonHeaderFieldsFrom: endianStream [
	"Read the fields shared by the local and central directory headers, from 'version
	needed to extract' to 'last mod file date' (APPNOTE 4.3.7 and 4.3.12)"

	versionNeededToExtract := endianStream nextLittleEndianNumber: 2.
	self bitFlag: (endianStream nextLittleEndianNumber: 2).
	compressionMethod := endianStream nextLittleEndianNumber: 2.
	lastModFileDateTime := self unzipTimestamp: (endianStream nextLittleEndianNumber: 4)
]

{ #category : 'private - reading' }
ZipFileMember >> readExtendedTimestamp [
	"An extended timestamp block gives the modification time in UTC with a precision of one second, better than the DOS time of the headers"

	((self extendedTimestampIn: cdExtraField) ifNil: [ self extendedTimestampIn: localExtraField ]) ifNotNil: [ :time |
		lastModFileDateTime := time ]
]

{ #category : 'private - reading' }
ZipFileMember >> readFrom: aStream [
	"assumes aStream positioned after CD header; leaves stream positioned after my CD entry"

	self readCentralDirectoryFileHeaderFrom: aStream.
	self readLocalDirectoryFileHeaderFrom: aStream.
	self readExtendedTimestamp.
	self endRead.
	self canonicalizeFileName
]

{ #category : 'private - reading' }
ZipFileMember >> readLocalDirectoryFileHeaderFrom: aStream [
	"Positions stream as necessary. Will return stream to its original position"

	| fileNameLength extraFieldLength xcrc32 xcompressedSize xuncompressedSize sig oldPos endianStream zip64Values fileNameBytes |
	oldPos := aStream position.

	aStream position: localHeaderRelativeOffset.

	sig := aStream next: 4.
	sig = LocalFileHeaderSignature asByteArray ifFalse: [
			aStream position: oldPos.
			^ ZipArchiveError signal: 'bad local header signature at ' , localHeaderRelativeOffset printStringHex ].

	endianStream := ZnEndianessReadWriteStream on: aStream.
	self readCommonHeaderFieldsFrom: endianStream.
	xcrc32 := endianStream nextLittleEndianNumber: 4.
	xcompressedSize := endianStream nextLittleEndianNumber: 4.
	xuncompressedSize := endianStream nextLittleEndianNumber: 4.

	fileNameLength := endianStream nextLittleEndianNumber: 2.
	extraFieldLength := endianStream nextLittleEndianNumber: 2.

	fileNameBytes := aStream next: fileNameLength.
	localExtraField := (aStream next: extraFieldLength) asByteArray.
	fileName := self fileNameFrom: fileNameBytes extraField: localExtraField.

	"With Zip64, both sizes are in the extended information extra field of the local header (APPNOTE 4.5.3)"
	zip64Values := self zip64ValuesIn: localExtraField.
	xuncompressedSize = Zip64Limit ifTrue: [ xuncompressedSize := self nextZip64ValueFrom: zip64Values ].
	xcompressedSize = Zip64Limit ifTrue: [ xcompressedSize := self nextZip64ValueFrom: zip64Values ].

	dataOffset := aStream position.

	"Don't trash these fields if we already got them from the central directory"
	self hasDataDescriptor ifFalse: [
			crc32 := xcrc32.
			compressedSize := xcompressedSize.
			uncompressedSize := xuncompressedSize ].

	aStream position: oldPos
]

{ #category : 'private - reading' }
ZipFileMember >> readRawChunk: n [
	^stream next: n
]

{ #category : 'private - reading' }
ZipFileMember >> readZip64ValuesFromCentralDirectory [
	"Sizes and offset that do not fit in the central directory header are in its Zip64 extended information extra field, in this order (APPNOTE 4.5.3)"

	| values |
	values := self zip64ValuesIn: cdExtraField.
	uncompressedSize = Zip64Limit ifTrue: [ uncompressedSize := self nextZip64ValueFrom: values ].
	compressedSize = Zip64Limit ifTrue: [ compressedSize := self nextZip64ValueFrom: values ].
	localHeaderRelativeOffset = Zip64Limit ifTrue: [ localHeaderRelativeOffset := self nextZip64ValueFrom: values ]
]

{ #category : 'private - reading' }
ZipFileMember >> rewindData [
	super rewindData.
	(stream isNil or: [ stream closed ])
		ifTrue: [ ZipArchiveError signal: 'the archive stream of ' , fileName printString , ' is missing or closed' ].
	stream position: (localHeaderRelativeOffset + 4).
	self skipLocalDirectoryFileHeaderFrom: stream
]

{ #category : 'private - reading' }
ZipFileMember >> skipLocalDirectoryFileHeaderFrom: aStream [
	"Assumes that stream is positioned after signature."

	|  extraFieldLength fileNameLength endianStream |
	aStream next: 22.

	endianStream := ZnEndianessReadWriteStream on: aStream.
	fileNameLength := endianStream nextLittleEndianNumber: 2.
	extraFieldLength := endianStream nextLittleEndianNumber: 2.
	aStream next: fileNameLength.
	aStream next: extraFieldLength.
	dataOffset := aStream position
]

{ #category : 'initialization' }
ZipFileMember >> stream: aStream externalFileName: aFileName [
	stream := aStream.
	externalFileName := aFileName
]

{ #category : 'private - writing' }
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
		aStream next: chunkSize putAll: buffer startingAt: 1.
		readDataRemaining := readDataRemaining - chunkSize.
	]] on: CRCError do: [ :ex | crcErrorMessage := ex messageText. ex resume ].

	crcErrorMessage ifNotNil: [ self isCorrupt: true. CRCError signal: crcErrorMessage ]
]

{ #category : 'private - writing' }
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
	]
]

{ #category : 'private - reading' }
ZipFileMember >> unzipTimestamp: dosTimestampInteger [

	^ [ DateAndTime fromDosTimestamp: dosTimestampInteger ]
		on: Error
		do: [ "Assume we're dealing with the old, incorrect format"
			DateAndTime fromSeconds: 2492992800 + dosTimestampInteger ]
]

{ #category : 'testing' }
ZipFileMember >> usesFile: aFileReferenceOrFileName [
	"Do I require aFileName? That is, do I care if it's clobbered?"
	^ externalFileName asFileReference = aFileReferenceOrFileName asFileReference
]
