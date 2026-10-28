"
A ZipArchive represents an archive that is read and/or written using the PKZIP file format.

ZipArchive instances know how to read and write such archives; their members are subinstances of ZipArchiveMember.
"
Class {
	#name : 'ZipArchive',
	#superclass : 'Archive',
	#instVars : [
		'centralDirectorySize',
		'centralDirectoryOffsetWRTStartingDiskNumber',
		'zipFileComment',
		'writeCentralDirectoryOffset',
		'writeEOCDOffset',
		'forceZip64',
		'writeExtendedTimestamps',
		'symbolicLinkHandler'
	],
	#pools : [
		'ZipFileConstants'
	],
	#category : 'Compression-Archives',
	#package : 'Compression',
	#tag : 'Archives'
}

{ #category : 'constants' }
ZipArchive class >> compressionDeflated [
	^CompressionDeflated
]

{ #category : 'constants' }
ZipArchive class >> compressionLevelDefault [
	^CompressionLevelDefault
]

{ #category : 'constants' }
ZipArchive class >> compressionLevelNone [
	^CompressionLevelNone
]

{ #category : 'constants' }
ZipArchive class >> compressionStored [
	^CompressionStored
]

{ #category : 'file in/out' }
ZipArchive class >> extractFrom: aZipFile to: aDirectory [

	^ self new
		  readFrom: aZipFile;
		  extractAllTo: aDirectory asFileReference
]

{ #category : 'constants' }
ZipArchive class >> findEndOfCentralDirectoryFrom: stream [
	"Seek in the given stream to the end, then read backwards until we find the
	signature of the central directory record. Leave the file positioned right
	before the signature.

	Answers the file position of the EOCD, or 0 if not found."

	| data fileLength seekOffset pos maxOffset |
	stream setToEnd.
	fileLength := stream position.
	"If the file length is less than 18 for the EOCD length plus 4 for the signature, we have a problem"
	fileLength < 22 ifTrue: [^ ZipArchiveError signal: 'file is too short to be a ZIP archive'].

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

{ #category : 'testing - file format' }
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

{ #category : 'constants' }
ZipArchive class >> validSignatures [
	"Return the valid signatures for a zip file"
	^Array
		with: LocalFileHeaderSignature
		with: CentralDirectoryFileHeaderSignature
		with: EndOfCentralDirectorySignature
]

{ #category : 'archive operations' }
ZipArchive >> addDeflateString: aString as: aFileName [
	"Add a verbatim string under the given file name"

	| mbr |
	mbr := self addString: aString as: aFileName.
	mbr desiredCompressionMethod: CompressionDeflated.
	^ mbr
]

{ #category : 'archive operations' }
ZipArchive >> addSymbolicLink: aTargetString as: aFileName [
	"Add a Unix symbolic link named aFileName pointing to aTargetString. The target
	is stored in UTF-8, uncompressed, as Info-ZIP does."

	| member |
	member := self addString: aTargetString as: aFileName.
	member desiredCompressionMethod: CompressionStored.
	member beSymbolicLink.
	^ member
]

{ #category : 'initialization' }
ZipArchive >> close [
	self members do: [:m | m close ]
]

{ #category : 'archive operations' }
ZipArchive >> extractAllTo: aDirectory [
	"Extract all elements to the given directory; notifying user via UI on progress and if existing files exist"

	[ : job |
		job max: self numberOfMembers.
		self extractAllTo: aDirectory informing: job ] 
			asJob run
]

{ #category : 'archive operations' }
ZipArchive >> extractAllTo: aDirectory informing: job [
	"Extract all members to aDirectory, keeping existing files; job, when not nil, is told about the progress"

	self extractAllTo: aDirectory informing: job overwrite: false
]

{ #category : 'archive operations' }
ZipArchive >> extractAllTo: aDirectory informing: job overwrite: overwriteAll [
	"Extract all members to aDirectory, directories first. Existing files are kept unless
	overwriteAll is true. job, when not nil, is told about the progress, at most every 100 ms."

	| progress count lastUpdate |
	progress := job ifNil: [ DummySystemProgressItem new ].
	count := 0.
	lastUpdate := 0.
	(self members select: #isDirectory) , (self members reject: #isDirectory) do: [ :entry |
			count := count + 1.
			(Time millisecondsSince: lastUpdate) >= 100 ifTrue: [
					lastUpdate := Time millisecondClockValue.
					progress
						title: (entry isDirectory
								 ifTrue: [ 'Creating ' ]
								 ifFalse: [ 'Extracting ' ]) , entry fileName;
						currentValue: count ].
			self extractMember: entry inDirectory: aDirectory overwrite: overwriteAll ]
]

{ #category : 'archive operations' }
ZipArchive >> extractAllTo: aDirectory overwrite: overwriteAll [
	"Extracts all elements to the given directory, overwriting existing entries if necessary. Does not use the UI for confirmation"

	self extractAllTo: aDirectory informing: nil overwrite: overwriteAll
]

{ #category : 'private' }
ZipArchive >> extractMember: aMember inDirectory: aDirectory overwrite: aBoolean [
	"Extract aMember under aDirectory. A symbolic link is given to my symbolicLinkHandler
	when I have one, once checked to stay inside aDirectory."

	| file |
	(symbolicLinkHandler isNotNil and: [ aMember isSymbolicLink ]) ifFalse: [
		^ aMember extractInDirectory: aDirectory overwrite: aBoolean ].
	file := aMember fileInDirectory: aDirectory.
	aMember ensureExtractionTarget: file isInside: aDirectory.
	^ symbolicLinkHandler value: aMember value: file
]

{ #category : 'accessing' }
ZipArchive >> forceZip64 [
	"Answer whether Zip64 records are written even when sizes, offsets and the number of members fit without them"

	^ forceZip64
]

{ #category : 'accessing' }
ZipArchive >> forceZip64: aBoolean [
	"When true, write Zip64 records (APPNOTE 4.5.3, 4.3.14 and 4.3.15) even when they are not needed.
	They are written anyway for members or archives larger than 4GB, or with more than 65534 members."

	forceZip64 := aBoolean
]

{ #category : 'accessing' }
ZipArchive >> hasMemberSuchThat: aBlock [
	"Answer whether we have a member satisfying the given condition"
	^self members anySatisfy: aBlock
]

{ #category : 'private' }
ZipArchive >> hasZip64LocatorIn: stream before: eocdPosition [

	eocdPosition < 20 ifTrue: [ ^ false ].
	stream position: eocdPosition - 20.
	^ (stream next: 4) asByteArray = Zip64EndOfCentralDirectoryLocatorSignature
]

{ #category : 'initialization' }
ZipArchive >> initialize [

	super initialize.
	writeEOCDOffset := writeCentralDirectoryOffset := 0.
	zipFileComment := ''.
	forceZip64 := false.
	writeExtendedTimestamps := false
]

{ #category : 'private' }
ZipArchive >> isSplitArchiveWithDiskNumber: diskNumber centralDirectoryDiskNumber: centralDirectoryDiskNumber entriesOnThisDisk: entriesOnThisDisk entries: entries [
	"Split (or spanned) archives are made of several files (.z01, .z02... .zip, APPNOTE 8.5): their last file has a disk number other than 0.
	16rFFFF means that the value is in the Zip64 end of central directory record (APPNOTE 4.4.1.4), so it is not considered."

	| known |
	known := [ :value | value ~= 16rFFFF ].
	^ ((known value: diskNumber) and: [ diskNumber ~= 0 ]) or: [
			  ((known value: centralDirectoryDiskNumber) and: [ centralDirectoryDiskNumber ~= 0 ]) or: [
				  (known value: entriesOnThisDisk) and: [ (known value: entries) and: [ entriesOnThisDisk ~= entries ] ] ] ]
]

{ #category : 'private' }
ZipArchive >> memberClass [
	^ZipArchiveMember
]

{ #category : 'private' }
ZipArchive >> needsZip64EndOfCentralDirectory [

	^ self forceZip64 or: [
			  members size >= Zip64EntriesLimit or: [
				  writeCentralDirectoryOffset >= Zip64Limit or: [ writeEOCDOffset - writeCentralDirectoryOffset >= Zip64Limit ] ] ]
]

{ #category : 'accessing' }
ZipArchive >> password: aStringOrByteArray [
	"Set the password of all my current members: to decrypt the encrypted ones,
	and to encrypt the others when I am written. Encryption uses the traditional
	PKWARE cipher (ZipCrypto), which is weak: see the comment of ZipCrypto."

	members do: [ :each | each password: aStringOrByteArray ]
]

{ #category : 'accessing' }
ZipArchive >> prependedDataSize [
	"Answer the size of whatever data exists before my first member.
	Assumes that I was read from a file or stream (i.e. the first member is a ZipFileMember)"
	^members isEmpty
		ifFalse: [ members first localHeaderRelativeOffset ]
		ifTrue: [ centralDirectoryOffsetWRTStartingDiskNumber ]
]

{ #category : 'private' }
ZipArchive >> readEndOfCentralDirectoryFrom: aStream [
	"Read EOCD, starting from position before signature."

	| signature zipFileCommentLength endianStream diskNumber centralDirectoryDiskNumber entriesOnThisDisk entries |
	signature := self readSignatureFrom: aStream.
	signature = EndOfCentralDirectorySignature ifFalse: [ ^ ZipArchiveError signal: 'bad end of central directory signature at ' , aStream position printString ].

	endianStream := ZnEndianessReadWriteStream on: aStream.
	diskNumber := endianStream nextLittleEndianNumber: 2. "# of this disk"
	centralDirectoryDiskNumber := endianStream nextLittleEndianNumber: 2. "# of disk with central dir start"
	entriesOnThisDisk := endianStream nextLittleEndianNumber: 2. "# of entries in central dir on this disk"
	entries := endianStream nextLittleEndianNumber: 2. "total # of entries in central dir"
	(self
		 isSplitArchiveWithDiskNumber: diskNumber
		 centralDirectoryDiskNumber: centralDirectoryDiskNumber
		 entriesOnThisDisk: entriesOnThisDisk
		 entries: entries) ifTrue: [ ZipArchiveError signal: 'split archives (made of several files) are not supported' ].
	centralDirectorySize := endianStream nextLittleEndianNumber: 4. "size of central directory"
	centralDirectoryOffsetWRTStartingDiskNumber := endianStream nextLittleEndianNumber: 4. "offset of start of central directory"
	zipFileCommentLength := endianStream nextLittleEndianNumber: 2. "zip file comment"
	zipFileComment := ZipArchiveMember decodeText: (aStream next: zipFileCommentLength)
]

{ #category : 'reading' }
ZipArchive >> readFrom: aStreamOrFile [

	| stream name eocdPosition |
	stream := aStreamOrFile isStream
		          ifTrue: [
				          name := aStreamOrFile printString.
				          aStreamOrFile ]
		          ifFalse: [
				          name := aStreamOrFile asFileReference.
				          aStreamOrFile asFileReference binaryReadStream ].
	eocdPosition := self class findEndOfCentralDirectoryFrom: stream.
	eocdPosition <= 0 ifTrue: [ ZipArchiveError signal: 'can''t find EOCD position' ].
	self readEndOfCentralDirectoryFrom: stream.
	stream position: (self readZip64EndOfCentralDirectoryFrom: stream before: eocdPosition).
	self readMembersFrom: stream named: name
]

{ #category : 'private' }
ZipArchive >> readMembersFrom: stream named: fileName [

	[
		| signature newMember |
		newMember := self memberClass newFromZipFile: stream named: fileName.
		signature := self readSignatureFrom: stream.
		(signature = EndOfCentralDirectorySignature or: [ signature = Zip64EndOfCentralDirectorySignature ]) ifTrue: [ ^ self ].
		signature = CentralDirectoryFileHeaderSignature ifFalse: [
			ZipArchiveError signal: 'bad CD signature at ' , (stream position - 4) printStringHex ].
		newMember readFrom: stream.
		newMember looksLikeDirectory ifTrue: [ newMember := newMember asDirectory ].
		self addMember: newMember ] repeat
]

{ #category : 'private' }
ZipArchive >> readSignatureFrom: stream [
	"Returns next signature from given stream, leaves stream positioned afterwards."

	| signatureData |
	signatureData := ByteArray new: 4.
	stream next: 4 into: signatureData.
	({
		 CentralDirectoryFileHeaderSignature.
		 LocalFileHeaderSignature.
		 EndOfCentralDirectorySignature.
		 Zip64EndOfCentralDirectorySignature } includes: signatureData) ifFalse: [
		^ ZipArchiveError signal: 'bad signature ' , signatureData asString asHex , ' at position ' , (stream position - 4) asString ].
	^ signatureData
]

{ #category : 'private' }
ZipArchive >> readZip64EndOfCentralDirectoryFrom: stream before: eocdPosition [
	"When the archive has a Zip64 end of central directory locator just before the end of central directory record, read the size and offset of the central directory from the Zip64 end of central directory record (APPNOTE 4.3.14 and 4.3.15).
	Answer the position where the central directory starts."

	| recordPosition endianStream |
	(self hasZip64LocatorIn: stream before: eocdPosition) ifFalse: [ ^ eocdPosition - centralDirectorySize ].
	recordPosition := self zip64RecordPositionIn: stream before: eocdPosition.
	stream position: recordPosition + 4.
	endianStream := ZnEndianessReadWriteStream on: stream.
	endianStream nextLittleEndianNumber: 8. "size of the record"
	endianStream nextLittleEndianNumber: 2. "version made by"
	endianStream nextLittleEndianNumber: 2. "version needed to extract"
	endianStream nextLittleEndianNumber: 4. "# of this disk"
	endianStream nextLittleEndianNumber: 4. "# of disk with central dir start"
	endianStream nextLittleEndianNumber: 8. "# of entries in central dir on this disk"
	endianStream nextLittleEndianNumber: 8. "total # of entries in central dir"
	centralDirectorySize := endianStream nextLittleEndianNumber: 8.
	centralDirectoryOffsetWRTStartingDiskNumber := endianStream nextLittleEndianNumber: 8.
	^ recordPosition - centralDirectorySize
]

{ #category : 'extraction' }
ZipArchive >> skipSymbolicLinks [
	"Do not extract members that are symbolic links"

	self symbolicLinkHandler: [ :member :file |  ]
]

{ #category : 'accessing' }
ZipArchive >> symbolicLinkHandler [

	^ symbolicLinkHandler
]

{ #category : 'accessing' }
ZipArchive >> symbolicLinkHandler: aBlock [
	"Set the block used by #extractAllTo: and its variants to extract members that
	are symbolic links. It is evaluated with the member and the file reference it
	would be extracted to; it can create the link with an operating system facility,
	or skip it. Consider checking #hasContainedSymbolicLinkTarget first, since links
	pointing outside the extraction directory are a classic archive attack.
	When nil, the default, a link is extracted as a regular file holding its target."

	symbolicLinkHandler := aBlock
]

{ #category : 'private' }
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

{ #category : 'private' }
ZipArchive >> writeEndOfCentralDirectoryTo: aStream [

	| endianStream centralDirectorySizeToWrite commentBytes |
	centralDirectorySizeToWrite := writeEOCDOffset - writeCentralDirectoryOffset.
	self needsZip64EndOfCentralDirectory ifTrue: [ self writeZip64EndOfCentralDirectoryTo: aStream ].
	aStream nextPutAll: EndOfCentralDirectorySignature.

	"Values that do not fit are written as their maximum, the real ones are in the Zip64 record"
	endianStream := ZnEndianessReadWriteStream on: aStream.
	endianStream nextLittleEndianNumber: 2 put: 0. "diskNumber"
	endianStream nextLittleEndianNumber: 2 put: 0. "diskNumberWithStartOfCentralDirectory"
	endianStream nextLittleEndianNumber: 2 put: (members size min: Zip64EntriesLimit). "numberOfCentralDirectoriesOnThisDisk"
	endianStream nextLittleEndianNumber: 2 put: (members size min: Zip64EntriesLimit). "numberOfCentralDirectories"
	endianStream nextLittleEndianNumber: 4 put: (centralDirectorySizeToWrite min: Zip64Limit). "size of central dir"
	endianStream nextLittleEndianNumber: 4 put: (writeCentralDirectoryOffset min: Zip64Limit). "offset of central dir"
	commentBytes := zipFileComment utf8Encoded.
	endianStream nextLittleEndianNumber: 2 put: commentBytes size. "zip file comment"

	aStream nextPutAll: commentBytes
]

{ #category : 'accessing' }
ZipArchive >> writeExtendedTimestamps [
	"Answer whether modification times are also written in extended timestamp extra fields"

	^ writeExtendedTimestamps
]

{ #category : 'accessing' }
ZipArchive >> writeExtendedTimestamps: aBoolean [
	"When true, also write the modification time of members in extended timestamp extra fields ('UT', 0x5455), in UTC with a precision of one second.
	Otherwise only the DOS time of the headers is written: local time, with a precision of two seconds."

	writeExtendedTimestamps := aBoolean
]

{ #category : 'writing' }
ZipArchive >> writeTo: stream [

	members do: [ :member |
			member writesExtendedTimestamp: self writeExtendedTimestamps.
			member writeTo: stream forcingZip64: self forceZip64.
			member endRead ].
	writeCentralDirectoryOffset := stream position.
	self writeCentralDirectoryTo: stream
]

{ #category : 'writing' }
ZipArchive >> writeTo: stream prepending: aString [
	stream nextPutAll: aString.
	self writeTo: stream
]

{ #category : 'writing' }
ZipArchive >> writeTo: stream prependingFile: aFileReferenceOrFileName [

	aFileReferenceOrFileName asFileReference binaryReadStreamDo: [ :prepended | | buffer |
	buffer := ByteArray new: (prepended size min: 32768).
	[ prepended atEnd ]
		whileFalse: [ | bytesRead |
			bytesRead := prepended
				readInto: buffer
				startingAt: 1
				count: buffer size.
			stream next: bytesRead putAll: buffer startingAt: 1 ].
		 ].

	self writeTo: stream
]

{ #category : 'private' }
ZipArchive >> writeZip64EndOfCentralDirectoryTo: aStream [
	"Write the Zip64 end of central directory record and its locator (APPNOTE 4.3.14 and 4.3.15). The record starts just after the central directory."

	| endianStream |
	aStream nextPutAll: Zip64EndOfCentralDirectorySignature.
	endianStream := ZnEndianessReadWriteStream on: aStream.
	endianStream nextLittleEndianNumber: 8 put: 44. "size of the rest of the record"
	endianStream nextLittleEndianNumber: 1 put: Zip64VersionNeededToExtract. "version made by"
	endianStream nextLittleEndianNumber: 1 put: FaUnix.
	endianStream nextLittleEndianNumber: 2 put: Zip64VersionNeededToExtract.
	endianStream nextLittleEndianNumber: 4 put: 0. "# of this disk"
	endianStream nextLittleEndianNumber: 4 put: 0. "# of disk with central dir start"
	endianStream nextLittleEndianNumber: 8 put: members size. "# of entries in central dir on this disk"
	endianStream nextLittleEndianNumber: 8 put: members size. "total # of entries in central dir"
	endianStream nextLittleEndianNumber: 8 put: writeEOCDOffset - writeCentralDirectoryOffset. "size of central dir"
	endianStream nextLittleEndianNumber: 8 put: writeCentralDirectoryOffset. "offset of central dir"

	aStream nextPutAll: Zip64EndOfCentralDirectoryLocatorSignature.
	endianStream nextLittleEndianNumber: 4 put: 0. "# of disk with the Zip64 record"
	endianStream nextLittleEndianNumber: 8 put: writeEOCDOffset. "offset of the Zip64 record"
	endianStream nextLittleEndianNumber: 4 put: 1 "total # of disks"
]

{ #category : 'private' }
ZipArchive >> zip64RecordPositionIn: stream before: eocdPosition [
	"The Zip64 end of central directory record is usually 56 bytes long and just before its locator, which also gives its offset.
	Trying the position before the locator first also works when data was prepended to the archive."

	| candidate offset |
	candidate := eocdPosition - 20 - 56.
	candidate >= 0 ifTrue: [
			stream position: candidate.
			(stream next: 4) asByteArray = Zip64EndOfCentralDirectorySignature ifTrue: [ ^ candidate ] ].
	stream position: eocdPosition - 20 + 4 + 4.
	offset := (ZnEndianessReadWriteStream on: stream) nextLittleEndianNumber: 8.
	stream position: offset.
	(stream next: 4) asByteArray = Zip64EndOfCentralDirectorySignature ifFalse: [
		ZipArchiveError signal: 'can''t find the Zip64 end of central directory record' ].
	^ offset
]

{ #category : 'accessing' }
ZipArchive >> zipFileComment [
	^zipFileComment asString
]

{ #category : 'accessing' }
ZipArchive >> zipFileComment: aString [
	zipFileComment := aString
]
