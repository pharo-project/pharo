"
A ZipArchive represents an archive that is read and/or written using the PKZIP file format.

ZipArchive instances know how to read and write such archives; their members are subinstances of ZipArchiveMember.
"
Class {
	#name : #ZipArchive,
	#superclass : #Archive,
	#instVars : [
		'centralDirectorySize',
		'centralDirectoryOffsetWRTStartingDiskNumber',
		'zipFileComment',
		'writeCentralDirectoryOffset',
		'writeEOCDOffset'
	],
	#pools : [
		'ZipFileConstants'
	],
	#category : #'Compression-Archives'
}

{ #category : #constants }
ZipArchive class >> compressionDeflated [
	^CompressionDeflated
]

{ #category : #constants }
ZipArchive class >> compressionLevelDefault [
	^CompressionLevelDefault
]

{ #category : #constants }
ZipArchive class >> compressionLevelNone [
	^CompressionLevelNone 
]

{ #category : #constants }
ZipArchive class >> compressionStored [
	^CompressionStored
]

{ #category : #'fileIn/Out' }
ZipArchive class >> extractAllIn: aFileName [
	"Service method to extract all contents of a zip."
	| directory |
	directory := (UIManager default chooseDirectoryFrom: aFileName asFileReference) ifNil: [^ self].
	^ (self new) 
		readFrom: aFileName;
		extractAllTo: directory.
]

{ #category : #constants }
ZipArchive class >> findEndOfCentralDirectoryFrom: stream [
	"Seek in the given stream to the end, then read backwards until we find the
	signature of the central directory record. Leave the file positioned right
	before the signature.

	Answers the file position of the EOCD, or 0 if not found."

	| data fileLength seekOffset pos maxOffset |
	stream setToEnd.
	fileLength := stream position.
	"If the file length is less than 18 for the EOCD length plus 4 for the signature, we have a problem"
	fileLength < 22 ifTrue: [^ self error: 'file is too short'].
	
	seekOffset := 0.
	pos := 0.
	data := ByteArray new: 4100.
	maxOffset := 40960 min: fileLength.	"limit search range to 40K"

	[
		seekOffset := (seekOffset + 4096) min: fileLength.
		stream position: fileLength - seekOffset.
		data := stream next: (4100 min: seekOffset) into: data startingAt: 1.
		pos := data lastIndexOfPKSignature: EndOfCentralDirectorySignature.
		pos = 0 and: [seekOffset < maxOffset]
	] whileTrue.

	^ pos > 0
		ifTrue: [ | newPos | stream position: (newPos := (stream position + pos - seekOffset - 1)). newPos]
		ifFalse: [0]
]

{ #category : #'file format' }
ZipArchive class >> isZipArchive: file [
	"Answer whether the given file represents a valid zip file. The argument file can be a String, FileReference or an open binary read stream.
	
	See ZipArchiveTest>>testisZipArchive for examples."

	| stream eocdPosition |
	stream := file isStream
		ifTrue: [ file ]
		ifFalse: [ file asFileReference binaryReadStream ].
		
	stream size < 22 ifTrue: [ ^ false ].
	
	eocdPosition := self findEndOfCentralDirectoryFrom: stream.
	stream ~= file ifTrue: [ stream close ].
	
	^ eocdPosition > 0
]

{ #category : #constants }
ZipArchive class >> validSignatures [
	"Return the valid signatures for a zip file"
	^Array 
		with: LocalFileHeaderSignature
		with: CentralDirectoryFileHeaderSignature
		with: EndOfCentralDirectorySignature
]

{ #category : #'archive operations' }
ZipArchive >> addDeflateString: aString as: aFileName [
	"Add a verbatim string under the given file name"

	| mbr |
	mbr := self addString: aString as: aFileName.
	mbr desiredCompressionMethod: CompressionDeflated.
	^ mbr
]

{ #category : #initialization }
ZipArchive >> close [
	self members do: [:m | m close ]
]

{ #category : #'archive operations' }
ZipArchive >> extractAllTo: aDirectory [
	"Extract all elements to the given directory; notifying user via UI on progress and if existing files exist"

	UIManager default
		informUserDuring: [:bar | 
			bar max: self numberOfMembers.
			self extractAllTo: aDirectory informing: bar ]
]

{ #category : #'archive operations' }
ZipArchive >> extractAllTo: aDirectory informing: bar [
	"Extract all elements to the given directory; notifying user via UI on progress and if existing files exist"

	^ self extractAllTo: aDirectory informing: bar overwrite: false
]

{ #category : #'archive operations' }
ZipArchive >> extractAllTo: aDirectory informing: aBar overwrite: allOverwrite [
	"Extract all elements to the given directory. Informs user when a file exists and set to overwrite"

	| bar overwriteAll barValue |
	bar := aBar ifNil: [ DummySystemProgressItem new ].
	overwriteAll := allOverwrite.
	barValue := 0.
	self members
		select: #isDirectory
		thenDo: [ :entry | 
			| dir shouldUpdateInfos lastUpdate |
			lastUpdate := 0.
			(shouldUpdateInfos := (Time millisecondsSince: lastUpdate) >= 100)
				ifTrue: [ lastUpdate := Time millisecondClockValue.
					bar label: 'Creating ' , entry fileName ].
			dir := (entry fileName findTokens: '/')
				inject: aDirectory
				into: [ :base :part | base / part ].
			dir ensureCreateDirectory.
			barValue := barValue + 1.
			shouldUpdateInfos
				ifTrue: [ bar value: barValue ] ].
	self members
		reject: #isDirectory
		thenDo: [ :entry | 
			| shouldUpdateInfos lastUpdate |
			lastUpdate := 0.
			(shouldUpdateInfos := (Time millisecondsSince: lastUpdate) >= 100)
				ifTrue: [ lastUpdate := Time millisecondClockValue.
					bar label: 'Extracting ' , entry fileName ].
			entry
				extractInDirectory: aDirectory
				informingOverwrite: overwriteAll
				onSuccess: [  ]
				onRetryWithOverwrite: [ overwriteAll := true ]
				onFailedOverwrite: [ (self confirm: 'Failed to extract ' , entry fileName , '. Proceed?')
						ifFalse: [ ^ self ] ]
				onAbortOverwrite: [ ^ self ].
			barValue := barValue + 1.
			shouldUpdateInfos
				ifTrue: [ bar value: barValue.
					lastUpdate := Time millisecondClockValue ] ]
]

{ #category : #'archive operations' }
ZipArchive >> extractAllTo: aDirectory overwrite: overwriteAll [
	"Extracts all elements to the given directory, overwriting existing entries if necessary. Does not use the UI for confirmation"

	self members do: [ :entry | entry extractInDirectory: aDirectory withoutInformingOverwrite: overwriteAll ]
]

{ #category : #accessing }
ZipArchive >> hasMemberSuchThat: aBlock [
	"Answer whether we have a member satisfying the given condition"
	^self members anySatisfy: aBlock
]

{ #category : #initialization }
ZipArchive >> initialize [
	super initialize.
	writeEOCDOffset := writeCentralDirectoryOffset := 0.
	zipFileComment := ''

]

{ #category : #private }
ZipArchive >> memberClass [
	^ZipArchiveMember
]

{ #category : #accessing }
ZipArchive >> prependedDataSize [
	"Answer the size of whatever data exists before my first member.
	Assumes that I was read from a file or stream (i.e. the first member is a ZipFileMember)"
	^members isEmpty
		ifFalse: [ members first localHeaderRelativeOffset ]
		ifTrue: [ centralDirectoryOffsetWRTStartingDiskNumber ]
]

{ #category : #private }
ZipArchive >> readEndOfCentralDirectoryFrom: aStream [
	"Read EOCD, starting from position before signature."
	| signature zipFileCommentLength endianStream |
	signature := self readSignatureFrom: aStream.
	signature = EndOfCentralDirectorySignature ifFalse: [ ^self error: 'bad signature at ', aStream position printString ].

	endianStream := ZnEndianessReadWriteStream on: aStream.
	endianStream nextLittleEndianNumber: 2. "# of this disk"
	endianStream nextLittleEndianNumber: 2. "# of disk with central dir start"
	endianStream nextLittleEndianNumber: 2. "# of entries in central dir on this disk"
	endianStream nextLittleEndianNumber: 2. "total # of entries in central dir"
	centralDirectorySize := endianStream nextLittleEndianNumber: 4. "size of central directory"
	centralDirectoryOffsetWRTStartingDiskNumber := endianStream nextLittleEndianNumber: 4. "offset of start of central directory"
	zipFileCommentLength := endianStream nextLittleEndianNumber: 2. "zip file comment"
	zipFileComment := aStream next: zipFileCommentLength

]

{ #category : #reading }
ZipArchive >> readFrom: aStreamOrFileName [
	| stream name eocdPosition |
	stream := aStreamOrFileName isStream
		ifTrue: [ name := aStreamOrFileName printString. aStreamOrFileName ]
		ifFalse: [ File openForReadFileNamed: (name := aStreamOrFileName asFileReference fullName) ].
	eocdPosition := self class findEndOfCentralDirectoryFrom: stream.
	eocdPosition <= 0 ifTrue: [ ZipArchiveError signal: 'can''t find EOCD position' ].
	self readEndOfCentralDirectoryFrom: stream.
	stream position: eocdPosition - centralDirectorySize.
	self readMembersFrom: stream named: name
]

{ #category : #private }
ZipArchive >> readMembersFrom: stream named: fileName [
	
	[ | signature newMember |
		newMember := self memberClass newFromZipFile: stream named: fileName.
		signature := self readSignatureFrom: stream.
		signature = EndOfCentralDirectorySignature ifTrue: [ ^self ].
		signature = CentralDirectoryFileHeaderSignature
			ifFalse: [ ZipArchiveError signal: 'bad CD signature at ', (stream position - 4) printStringHex ].
		newMember readFrom: stream.
		newMember looksLikeDirectory ifTrue: [ newMember := newMember asDirectory ].
		self addMember: newMember.
	] repeat
]

{ #category : #private }
ZipArchive >> readSignatureFrom: stream [
	"Returns next signature from given stream, leaves stream positioned afterwards."

	| signatureData | 
	signatureData := ByteArray new: 4.
	stream next: 4 into: signatureData.
	({ CentralDirectoryFileHeaderSignature . LocalFileHeaderSignature . EndOfCentralDirectorySignature }
		includes: signatureData)
			ifFalse: [ ^ ZipArchiveError signal: 'bad signature ', signatureData asString asHex, ' at position ', (stream position - 4) asString ].
	^signatureData

]

{ #category : #private }
ZipArchive >> writeCentralDirectoryTo: aStream [
	| offset |
	offset := writeCentralDirectoryOffset.
	members do: [ :member |
		member writeCentralDirectoryFileHeaderTo: aStream.
		offset := offset + member centralDirectoryHeaderSize.
	].
	writeEOCDOffset := offset.
	self writeEndOfCentralDirectoryTo: aStream


]

{ #category : #private }
ZipArchive >> writeEndOfCentralDirectoryTo: aStream [

	| endianStream |
	aStream nextPutAll: EndOfCentralDirectorySignature.
	
	endianStream := ZnEndianessReadWriteStream on: aStream.
	endianStream nextLittleEndianNumber: 2 put: 0. "diskNumber"
	endianStream nextLittleEndianNumber: 2 put: 0. "diskNumberWithStartOfCentralDirectory"
	endianStream nextLittleEndianNumber: 2 put: members size. "numberOfCentralDirectoriesOnThisDisk"
	endianStream nextLittleEndianNumber: 2 put: members size. "numberOfCentralDirectories"
	endianStream nextLittleEndianNumber: 4 put: writeEOCDOffset - writeCentralDirectoryOffset. "size of central dir"
	endianStream nextLittleEndianNumber: 4 put: writeCentralDirectoryOffset. "offset of central dir"
	endianStream nextLittleEndianNumber: 2 put: zipFileComment size. "zip file comment"
	
	zipFileComment isEmpty ifFalse: [ aStream nextPutAll: zipFileComment ]


]

{ #category : #writing }
ZipArchive >> writeTo: stream [
	members do: [ :member |
		member writeTo: stream.
		member endRead.
	].
	writeCentralDirectoryOffset := stream position.
	self writeCentralDirectoryTo: stream
	
]

{ #category : #writing }
ZipArchive >> writeTo: stream prepending: aString [
	stream nextPutAll: aString.
	members do: [ :member |
		member writeTo: stream.
		member endRead.
	].
	writeCentralDirectoryOffset := stream position.
	self writeCentralDirectoryTo: stream
	
]

{ #category : #writing }
ZipArchive >> writeTo: stream prependingFileNamed: aFileName [
	| prepended buffer |
	prepended := File openForReadFileNamed: aFileName.
	buffer := ByteArray new: (prepended size min: 32768).
	[ prepended atEnd ] whileFalse: [ | bytesRead |
		bytesRead := prepended readInto: buffer startingAt: 1 count: buffer size.
		stream next: bytesRead putAll: buffer startingAt: 1
	].
	members do: [ :member |
		member writeTo: stream.
		member endRead.
	].
	writeCentralDirectoryOffset := stream position.
	self writeCentralDirectoryTo: stream
	
]

{ #category : #writing }
ZipArchive >> writeToFileNamed: aFileName prepending: aString [
	
	"Catch attempts to overwrite existing zip file"
	(self canWriteToFileNamed: aFileName)
		ifFalse: [ ^ ZipArchiveError signal: (aFileName, ' is needed by one or more members in this archive') ].
	
	(File named: aFileName)
		writeStreamDo: [ :stream | self writeTo: stream prepending: aString ]	
]

{ #category : #writing }
ZipArchive >> writeToFileNamed: aFileName prependingFileNamed: anotherFileName [
	
	"Catch attempts to overwrite existing zip file"
	(self canWriteToFileNamed: aFileName)
		ifFalse: [ ^ZipArchiveError signal: (aFileName, ' is needed by one or more members in this archive') ].
	
	(File named: aFileName)
		writeStreamDo: [ :stream | self writeTo: stream prependingFileNamed: anotherFileName ]	
]

{ #category : #accessing }
ZipArchive >> zipFileComment [
	^zipFileComment asString
]

{ #category : #accessing }
ZipArchive >> zipFileComment: aString [
	zipFileComment := aString
]
