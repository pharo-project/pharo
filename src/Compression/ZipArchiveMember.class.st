"
Subinstances of me are members in a ZipArchive.

They represent different data sources:
* ZipDirectoryMember -- a directory to be added to a zip file
* ZipFileMember -- a file or directory that is already in a zip file
* ZipNewFilemember -- a file that is to be added to a zip file
* ZipStringMember -- a string that is to be added to a zip file

They can write their data to another stream either copying, compressing,
or decompressing as desired.
"
Class {
	#name : 'ZipArchiveMember',
	#superclass : 'ArchiveMember',
	#instVars : [
		'lastModFileDateTime',
		'fileAttributeFormat',
		'versionMadeBy',
		'versionNeededToExtract',
		'bitFlag',
		'compressionMethod',
		'desiredCompressionMethod',
		'desiredCompressionLevel',
		'internalFileAttributes',
		'externalFileAttributes',
		'cdExtraField',
		'localExtraField',
		'fileComment',
		'crc32',
		'compressedSize',
		'uncompressedSize',
		'readDataRemaining',
		'headerStartPosition',
		'usesZip64',
		'writesExtendedTimestamp'
	],
	#pools : [
		'ZipFileConstants'
	],
	#category : 'Compression-Archives',
	#package : 'Compression',
	#tag : 'Archives'
}

{ #category : 'accessing' }
ZipArchiveMember class >> cp437HighCodePoints [
	"Unicode code points of the bytes 128 to 255 of IBM code page 437, the encoding of ZIP file names and comments when the language encoding flag is not set (APPNOTE, appendix D)"

	^ #( 199 252 233 226 228 224 229 231 234 235 232 239 238 236 196 197 201 230 198 244 246 242 251 249 255 214 220 162 163 165 8359
	     402 225 237 243 250 241 209 170 186 191 8976 172 189 188 161 171 187 9617 9618 9619 9474 9508 9569 9570 9558 9557 9571 9553
	     9559 9565 9564 9563 9488 9492 9524 9516 9500 9472 9532 9566 9567 9562 9556 9577 9574 9568 9552 9580 9575 9576 9572 9573 9561
	     9560 9554 9555 9579 9578 9496 9484 9608 9604 9612 9616 9600 945 223 915 960 931 963 181 964 934 920 937 948 8734 966 949 8745
	     8801 177 8805 8804 8992 8993 247 8776 176 8729 183 8730 8319 178 9632 160 )
]

{ #category : 'instance creation' }
ZipArchiveMember class >> newFromDirectory: aFileReference localName: localName [
	^ZipDirectoryMember newFromDirectory: aFileReference localName: localName
]

{ #category : 'instance creation' }
ZipArchiveMember class >> newFromFile: aFileReference [
	^ZipNewFileMember newFromFile: aFileReference
]

{ #category : 'instance creation' }
ZipArchiveMember class >> newFromString: aString named: aFileName [
	^ZipStringMember newFrom: aString named: aFileName
]

{ #category : 'instance creation' }
ZipArchiveMember class >> newFromZipFile: stream named: fileName [
	^ZipFileMember newFrom: stream named: fileName
]

{ #category : 'private' }
ZipArchiveMember >> asDirectory [
	^ZipDirectoryMember new copyFrom: self
]

{ #category : 'accessing - spec fields' }
ZipArchiveMember >> bitFlag [
	"Per .ZIP File Format Specification Version: 6.3.2:
		2 bytes:
		- Bit 0: If set, indicates that the file is encrypted.

		(For Method 6 - Imploding)
		- Bit 1: If the compression method used was type 6, Imploding, then this bit, if set, indicates an 8K sliding dictionary was used.  If clear, then a 4K sliding dictionary was used.
		- Bit 2: If the compression method used was type 6, Imploding, then this bit, if set, indicates 3 Shannon-Fano trees were used to encode the sliding dictionary output.  If clear, then 2 Shannon-Fano trees were used.

		(For Methods 8 and 9 - Deflating)
		Bit 2  Bit 1
		0      0    Normal (-en) compression option was used.
		0      1    Maximum (-exx/-ex) compression option was used.
		1      0    Fast (-ef) compression option was used.
		1      1    Super Fast (-es) compression option was used.

		(For Method 14 - LZMA)
		Bit 1: If the compression method used was type 14, LZMA, then this bit, if set, indicates an end-of-stream (EOS) marker is used to mark the end of the compressed data stream. If clear, then an EOS marker is not present and the compressed data size must be known to extract.

		Note:  Bits 1 and 2 are undefined if the compression method is any other.

		Bit 3: If this bit is set, the fields crc-32, compressed size and uncompressed size are set to zero in the local header.  The correct values are put in the data descriptor immediately following the compressed data.  (Note: PKZIP version 2.04g for DOS only recognizes this bit for method 8 compression, newer versions of PKZIP recognize this bit for any compression method.)

		Bit 4: Reserved for use with method 8, for enhanced deflating.

		Bit 5: If this bit is set, this indicates that the file is compressed patched data.  (Note: Requires PKZIP version 2.70 or greater)

		Bit 6: Strong encryption.  If this bit is set, you should set the version needed to extract value to at least 50 and you must also set bit 0.  If AES encryption is used, the version needed to extract value must be at least 51.

		Bit 7: Currently unused.

		Bit 8: Currently unused.

		Bit 9: Currently unused.

		Bit 10: Currently unused.

		Bit 11: Language encoding flag (EFS).  If this bit is set, the filename and comment fields for this file must be encoded using UTF-8. (see APPENDIX D)

		Bit 12: Reserved by PKWARE for enhanced compression.

		Bit 13: Used when encrypting the Central Directory to indicate selected data values in the Local Header are masked to hide their actual values.  See the section describing the Strong Encryption Specification for details.

		Bit 14: Reserved by PKWARE.

		Bit 15: Reserved by PKWARE."

	^ bitFlag
]

{ #category : 'accessing - spec fields' }
ZipArchiveMember >> bitFlag: aNumber [
	"See comment for #bitFlag"

	bitFlag := aNumber
]

{ #category : 'private - encoding' }
ZipArchiveMember >> bitFlagToWrite [
	"Answer my general purpose bit flag, with the language encoding flag when my file name or comment is not ASCII, since they are written in UTF-8"

	^ (fileName isAsciiString and: [ fileComment isAsciiString ])
		  ifTrue: [ self bitFlag ]
		  ifFalse: [ self bitFlag bitOr: LanguageEncodingFlag ]
]

{ #category : 'accessing - extra fields' }
ZipArchiveMember >> centralDirectoryExtraField [
	"Answer the raw extra field of my central directory header (APPNOTE 4.5)"

	^ cdExtraField asByteArray
]

{ #category : 'accessing - extra fields' }
ZipArchiveMember >> centralDirectoryExtraField: aByteArray [
	"Set the raw extra field of my central directory header (APPNOTE 4.5). Zip64 and extended timestamp blocks are updated when I am written."

	cdExtraField := aByteArray asByteArray
]

{ #category : 'private - zip64' }
ZipArchiveMember >> centralDirectoryExtraFieldToWrite [
	"With Zip64, the central directory header has both sizes and the offset of the local header in a Zip64 extended information extra field"

	^ self extraFieldToWriteFrom: cdExtraField zip64Values: (self usesZip64
			   ifTrue: [
					   {
						   uncompressedSize.
						   self compressedSizeToWrite.
						   headerStartPosition } ]
			   ifFalse: [ #(  ) ])
]

{ #category : 'accessing' }
ZipArchiveMember >> centralDirectoryHeaderSize [

	| systemFileName systemFileComment systemCdExtraField |
	systemFileName := fileName asVmPathName.
	systemFileComment := fileComment utf8Encoded.
	systemCdExtraField := self centralDirectoryExtraFieldToWrite.
	^ 46 + systemFileName size + systemCdExtraField size + systemFileComment size
]

{ #category : 'accessing' }
ZipArchiveMember >> clearExtraFields [
	cdExtraField := ''.
	localExtraField := ''
]

{ #category : 'private - writing' }
ZipArchiveMember >> compressDataTo: aStream [
	"Copy my deflated data to the given stream."
	| encoder startPos endPos |

	encoder := ZipWriteStream on: aStream.
	startPos := aStream position.

	[ readDataRemaining > 0 ] whileTrue: [ | data |
		data := self readRawChunk: (4096 min: readDataRemaining).
		encoder nextPutAll: data asByteArray.
		readDataRemaining := readDataRemaining - data size.
	].
	encoder finish. "not close!"
	endPos := aStream position.
	compressedSize := endPos - startPos.
	crc32 := encoder crc
]

{ #category : 'accessing' }
ZipArchiveMember >> compressedSize [
	"Return the compressed size for this member.
	This will not be set for members that were constructed from strings
	or external files until after the member has been written."
	^compressedSize
]

{ #category : 'private - zip64' }
ZipArchiveMember >> compressedSizeToWrite [

	^ desiredCompressionMethod = CompressionStored
		  ifTrue: [ uncompressedSize ]
		  ifFalse: [ compressedSize ]
]

{ #category : 'accessing' }
ZipArchiveMember >> compressionMethod [
	"Returns my compression method. This is the method that is
	currently being used to compress my data.

	This will be CompressionStored for added string or file members,
	or CompressionStored or CompressionDeflated (others are possible but not handled)"

	^compressionMethod
]

{ #category : 'accessing' }
ZipArchiveMember >> contentStream [
	"Answer my contents as a text stream.
	Default is no conversion, since we don't know what the bytes mean."

	^self contentStreamFromEncoding: 'latin1'
]

{ #category : 'accessing' }
ZipArchiveMember >> contentStreamFromEncoding: encodingName [
	"Answer my contents as a text stream.
	Interpret the raw bytes with given encodingName"

	^ ((ByteArray new: self uncompressedSize streamContents: [ :stream |
		self extractTo: stream ]) decodeWith: encodingName)
			readStream
]

{ #category : 'reading' }
ZipArchiveMember >> contents [
	"Answer my contents as a string."
	^ ByteArray new: self uncompressedSize streamContents: [ :s |
		self extractTo: s ]
]

{ #category : 'reading' }
ZipArchiveMember >> contentsFrom: start to: finish [
	"Answer my contents as a string."
	| s |
	s := (String new: finish - start + 1) writeStream.
	self extractTo: s from: start to: finish.
	^s contents
]

{ #category : 'private - writing' }
ZipArchiveMember >> copyDataTo: aStream [

	compressionMethod = CompressionStored ifTrue: [ ^self copyDataWithCRCTo: aStream ].

	self copyRawDataTo: aStream
]

{ #category : 'private - writing' }
ZipArchiveMember >> copyDataWithCRCTo: aStream [
	"Copy my data to aStream. Also set the CRC-32.
	Only used when compressionMethod = desiredCompressionMethod = CompressionStored"

	uncompressedSize := compressedSize := readDataRemaining.

	crc32 := 16rFFFFFFFF.

	[ readDataRemaining > 0 ] whileTrue: [ | data |
		data := self readRawChunk: (4096 min: readDataRemaining).
		aStream nextPutAll: data.
		crc32 := ZipWriteStream updateCrc: crc32 from: 1 to: data size in: data.
		readDataRemaining := readDataRemaining - data size.
	].

	crc32 := crc32 bitXor: 16rFFFFFFFF
]

{ #category : 'private - writing' }
ZipArchiveMember >> copyRawDataTo: aStream [

	[ readDataRemaining > 0 ] whileTrue: [ | data |
		data := self readRawChunk: (4096 min: readDataRemaining).
		aStream nextPutAll: data.
		readDataRemaining := readDataRemaining - data size ]
]

{ #category : 'private - writing' }
ZipArchiveMember >> copyRawDataTo: aStream from: start to: finish [

	readDataRemaining := readDataRemaining min: finish - start + 1.

	self readRawChunk: start - 1.

	[ readDataRemaining > 0 ] whileTrue: [ | data |
		data := self readRawChunk: (32768 min: readDataRemaining).
		aStream nextPutAll: data.
		readDataRemaining := readDataRemaining - data size ]
]

{ #category : 'accessing' }
ZipArchiveMember >> crc32 [
	^crc32
]

{ #category : 'accessing' }
ZipArchiveMember >> crc32String [
	| hexString |
	hexString := crc32 storeStringHex.
	^('00000000' copyFrom: 1 to: 11 - (hexString size)) , (hexString copyFrom: 4 to: hexString size)
]

{ #category : 'private - encoding' }
ZipArchiveMember >> decodeCp437: aByteArray [

	| highCodePoints |
	highCodePoints := self class cp437HighCodePoints.
	^ String streamContents: [ :stream |
			  aByteArray do: [ :each |
					  stream nextPut: (Character value: (each < 128
								    ifTrue: [ each ]
								    ifFalse: [ highCodePoints at: each - 127 ])) ] ]
]

{ #category : 'private - encoding' }
ZipArchiveMember >> decodeText: aByteArray [
	"Decode a file name or comment read from the archive (APPNOTE, appendix D).
	With the language encoding flag the text is UTF-8, otherwise it should be CP437.
	Many tools (and older versions of Pharo) write UTF-8 without the flag, so valid UTF-8 is decoded as UTF-8 anyway."

	| bytes |
	bytes := aByteArray asByteArray.
	((self bitFlag anyMask: LanguageEncodingFlag) or: [ bytes allSatisfy: [ :each | each < 128 ] ]) ifTrue: [ ^ bytes utf8Decoded ].
	^ [ bytes utf8Decoded ]
		  on: ZnCharacterEncodingError
		  do: [ self decodeCp437: bytes ]
]

{ #category : 'accessing' }
ZipArchiveMember >> desiredCompressionLevel [
	^desiredCompressionLevel
]

{ #category : 'accessing' }
ZipArchiveMember >> desiredCompressionLevel: aNumber [
	"Set my desiredCompressionLevel
	This is the method that will be used to write.
	Returns prior desiredCompressionLevel.

	Valid arguments are 0 (CompressionLevelNone) through 9,
	including 6 (CompressionLevelDefault).

	0 (CompressionLevelNone) will change the desiredCompressionMethod
	to CompressionStored. All other arguments will change the
	desiredCompressionMethod to CompressionDeflated."

	| old |
	old := desiredCompressionLevel.
	desiredCompressionLevel := aNumber.
	desiredCompressionMethod := (aNumber > 0)
		ifTrue: [ CompressionDeflated ]
		ifFalse: [ CompressionStored ].
	^old
]

{ #category : 'accessing' }
ZipArchiveMember >> desiredCompressionMethod [
	"Get my desiredCompressionMethod.
	This is the method that will be used to write"

	^desiredCompressionMethod
]

{ #category : 'accessing' }
ZipArchiveMember >> desiredCompressionMethod: aNumber [
	"Set my desiredCompressionMethod
	This is the method that will be used to write.
	Answers prior desiredCompressionMethod.

	Only CompressionDeflated or CompressionStored are valid arguments.

	Changing to CompressionStored will change my desiredCompressionLevel
	to CompressionLevelNone; changing to CompressionDeflated will change my
	desiredCompressionLevel to CompressionLevelDefault."

	| old |
	self flag: 'should be an error if unsupported, but valid zip compression is requested. Also, clean up test branches here'.
	old := desiredCompressionMethod.
	desiredCompressionMethod := aNumber.
	desiredCompressionLevel := (aNumber = CompressionDeflated)
			ifTrue: [ CompressionLevelDefault ]
			ifFalse: [ CompressionLevelNone ].
	compressionMethod = CompressionStored ifTrue: [ compressedSize := uncompressedSize ].
	^old
]

{ #category : 'private' }
ZipArchiveMember >> endRead [
	readDataRemaining := 0
]

{ #category : 'private - extra fields' }
ZipArchiveMember >> extendedTimestampData [
	"Answer the data of an extended timestamp block holding my modification time, or nil if it does not fit in a signed 4 bytes number"

	| seconds |
	seconds := lastModFileDateTime asDateAndTime asUnixTime.
	(seconds between: -16r80000000 and: 16r7FFFFFFF) ifFalse: [ ^ nil ].
	seconds < 0 ifTrue: [ seconds := seconds + 16r100000000 ].
	^ ByteArray streamContents: [ :stream |
			  stream nextPut: 1. "only the modification time"
			  0 to: 3 do: [ :index | stream nextPut: ((seconds bitShift: index * -8) bitAnd: 255) ] ]
]

{ #category : 'private - extra fields' }
ZipArchiveMember >> extendedTimestampIn: aByteArrayOrString [
	"Answer the modification time of an extended timestamp block ('UT', 0x5455), or nil.
	Its first byte tells which times follow; the modification time is first, as a signed 4 bytes number of seconds since 1970 UTC."

	| data seconds |
	data := self extraFieldIn: aByteArrayOrString withId: ExtendedTimestampExtraFieldId.
	(data isNil or: [ data size < 5 or: [ ((data at: 1) bitAnd: 1) = 0 ] ]) ifTrue: [ ^ nil ].
	seconds := (5 to: 2 by: -1) inject: 0 into: [ :value :index | (value bitShift: 8) + (data at: index) ].
	seconds >= 16r80000000 ifTrue: [ seconds := seconds - 16r100000000 ].
	^ DateAndTime fromUnixTime: seconds
]

{ #category : 'private - extra fields' }
ZipArchiveMember >> extraField: aByteArrayOrString replacing: anId with: aByteArrayOrNil [
	"Answer aByteArrayOrString without its blocks of header id anId, followed by a new block of that id when aByteArrayOrNil is not nil.
	When there is nothing to remove or add, answer the extra field unchanged."

	| fields |
	(aByteArrayOrNil isNil and: [ (self extraFieldIn: aByteArrayOrString withId: anId) isNil ]) ifTrue: [
		^ aByteArrayOrString asByteArray ].
	fields := (self extraFieldsIn: aByteArrayOrString) reject: [ :each | each key = anId ].
	aByteArrayOrNil ifNotNil: [ fields add: anId -> aByteArrayOrNil ].
	^ self extraFieldBytesFrom: fields
]

{ #category : 'private - extra fields' }
ZipArchiveMember >> extraFieldBytesFrom: aCollectionOfAssociations [
	"Answer the extra field made of the given associations from header id to data"

	^ ByteArray streamContents: [ :stream |
			  aCollectionOfAssociations do: [ :each |
					  stream
						  nextPut: (each key bitAnd: 255);
						  nextPut: (each key bitShift: -8);
						  nextPut: (each value size bitAnd: 255);
						  nextPut: (each value size bitShift: -8);
						  nextPutAll: each value ] ]
]

{ #category : 'private - extra fields' }
ZipArchiveMember >> extraFieldIn: aByteArrayOrString withId: anId [
	"Answer the data of the block of header id anId, or nil"

	^ (self extraFieldsIn: aByteArrayOrString) detect: [ :each | each key = anId ] ifFound: [ :each | each value ] ifNone: [ nil ]
]

{ #category : 'private - extra fields' }
ZipArchiveMember >> extraFieldToWriteFrom: anExtraField zip64Values: aCollection [
	"Answer anExtraField with the Zip64 extended information holding aCollection, and with my modification time in an extended timestamp block when I write one or already have one"

	| extraField |
	extraField := self extraField: anExtraField replacing: Zip64ExtraFieldId with: (self zip64DataFor: aCollection).
	(writesExtendedTimestamp == true or: [ (self extraFieldIn: extraField withId: ExtendedTimestampExtraFieldId) isNotNil ]) ifTrue: [
		extraField := self extraField: extraField replacing: ExtendedTimestampExtraFieldId with: self extendedTimestampData ].
	^ extraField
]

{ #category : 'private - extra fields' }
ZipArchiveMember >> extraFieldsIn: aByteArrayOrString [
	"Answer the blocks of an extra field (APPNOTE 4.5.1) as associations from header id to data. Truncated blocks are ignored."

	| bytes fields index |
	bytes := aByteArrayOrString asByteArray.
	fields := OrderedCollection new.
	index := 1.
	[ index + 3 <= bytes size ] whileTrue: [
			| id size |
			id := (bytes at: index) + ((bytes at: index + 1) bitShift: 8).
			size := (bytes at: index + 2) + ((bytes at: index + 3) bitShift: 8).
			index + 3 + size > bytes size ifTrue: [ ^ fields ].
			fields add: id -> (bytes copyFrom: index + 4 to: index + 3 + size).
			index := index + 4 + size ].
	^ fields
]

{ #category : 'extraction-ui' }
ZipArchiveMember >> extractInDirectory: aDirectory [
	"Extract this entry into the given directory. Answer successBlock or call one of the other action blocks."

	| path fileDir file localName |
	path := fileName findTokens: '/'.
	localName := path last.
	fileDir := path allButLast
		           inject: aDirectory
		           into: [ :base :part | base / part ].
	fileDir ensureCreateDirectory.
	file := fileDir / localName.
	^ self extractToFile: file
]

{ #category : 'extraction' }
ZipArchiveMember >> extractInDirectory: dir withoutInformingOverwrite: overwrite [
	"Extract myself to dir without informing user whether to overwrite existing file"

	self extractToFileNamed: self localFileName inDirectory: dir overwrite: overwrite
]

{ #category : 'extraction' }
ZipArchiveMember >> extractTo: aStream [
	| oldCompression |
	self isEncrypted ifTrue: [ self error: 'encryption is unsupported' ].
	oldCompression := self desiredCompressionMethod: CompressionStored.
	self rewindData.
	self writeDataTo: aStream.
	self desiredCompressionMethod: oldCompression.
	self endRead
]

{ #category : 'extraction' }
ZipArchiveMember >> extractTo: aStream from: start to: finish [
	| oldCompression |
	self isEncrypted ifTrue: [ self error: 'encryption is unsupported' ].
	aStream binary.
	oldCompression := self desiredCompressionMethod: CompressionStored.
	self rewindData.
	self writeDataTo: aStream from: start to: finish.
	self desiredCompressionMethod: oldCompression.
	self endRead
]

{ #category : 'private - extraction' }
ZipArchiveMember >> extractToFile: aFile [
	"Extract this entry into the given file. Answer successBlock or call one of the other action blocks."

	| file |
	file := aFile.
	file exists ifTrue: [ ^ self ].
	file binaryWriteStreamDo: [ :str | self extractTo: str ]
]

{ #category : 'extraction' }
ZipArchiveMember >> extractToFileNamed: aFileName [
	self extractToFileNamed: aFileName inDirectory: FileSystem workingDirectory overwrite: true
]

{ #category : 'extraction' }
ZipArchiveMember >> extractToFileNamed: aLocalFileName inDirectory: dir [
	self extractToFileNamed: aLocalFileName inDirectory: dir overwrite: true
]

{ #category : 'extraction' }
ZipArchiveMember >> extractToFileNamed: aLocalFileName inDirectory: dir overwrite: overwrite [
	| file |
	self isEncrypted
		ifTrue: [ ^ self error: 'encryption unsupported' ].
	file := dir / aLocalFileName.
	self isDirectory
		ifTrue: [ file ensureCreateDirectory ]
		ifFalse: [ (file isFile and: [ file exists ])
				ifTrue: [ overwrite
						ifTrue: [ file
								ensureDelete;
								ensureCreateFile;
								binaryWriteStreamDo: [ :stream | self extractTo: stream ] ] ]
				ifFalse: [ file
						ensureCreateFile;
						binaryWriteStreamDo: [ :stream | self extractTo: stream ] ] ]
]

{ #category : 'extraction' }
ZipArchiveMember >> extractWithOverwriteInDirectory: dir [
	self extractToFileNamed: self localFileName inDirectory: dir overwrite: true
]

{ #category : 'accessing' }
ZipArchiveMember >> fileComment [
	^fileComment
]

{ #category : 'accessing' }
ZipArchiveMember >> fileComment: aString [
	fileComment := aString
]

{ #category : 'private - extra fields' }
ZipArchiveMember >> fileNameFrom: aByteArray extraField: anExtraField [
	"Decode the file name of a header, preferring the UTF-8 name of a matching Unicode path block"

	^ (self unicodePathIn: anExtraField forNameBytes: aByteArray) ifNil: [ self decodeText: aByteArray ]
]

{ #category : 'testing' }
ZipArchiveMember >> hasDataDescriptor [
	^ (self bitFlag bitAnd: 8)	~= 0 "GPBF:=HAS:=DATA:=DESCRIPTOR:=MASK"
]

{ #category : 'initialization' }
ZipArchiveMember >> initialize [
	super initialize.
	lastModFileDateTime := DateAndTime epoch.
	fileAttributeFormat := FaUnix.
	self versionMadeBy: 20. "Dos-compatible file attributes; ZIP v. 2.0. See comment for #versionMadeBy"
	self versionNeededToExtract: 20. "ZIP v. 2.0. See comment for #versionNeededToExtract"
	self bitFlag: 0.
	compressionMethod := CompressionStored.
	desiredCompressionMethod := CompressionDeflated.
	desiredCompressionLevel := CompressionLevelDefault.
	internalFileAttributes := 0.
	externalFileAttributes := 0.
	fileName := ''.
	cdExtraField := ''.
	localExtraField := ''.
	fileComment := ''.
	crc32 := 0.
	compressedSize := 0.
	uncompressedSize := 0.
	self unixFileAttributes: DefaultFilePermissions
]

{ #category : 'testing' }
ZipArchiveMember >> isDirectory [
	^false
]

{ #category : 'testing' }
ZipArchiveMember >> isEncrypted [
	"Return true if this member is encrypted (this is unsupported)"
	^ (self bitFlag bitAnd: 1) ~= 0
]

{ #category : 'testing' }
ZipArchiveMember >> isTextFile [
	"Returns true if I am a text file.
	Note that this module does not currently do anything with this flag
	upon extraction or storage.
	That is, bytes are stored in native format whether or not they came
	from a text file."
	^ (internalFileAttributes bitAnd: 1) ~= 0
]

{ #category : 'testing' }
ZipArchiveMember >> isTextFile: aBoolean [
	"Set whether I am a text file.
	Note that this module does not currently do anything with this flag
	upon extraction or storage.
	That is, bytes are stored in native format whether or not they came
	from a text file."
	internalFileAttributes := aBoolean
		ifTrue: [ internalFileAttributes bitOr: 1 ]
		ifFalse: [ internalFileAttributes bitAnd: 1 bitInvert ]
]

{ #category : 'accessing' }
ZipArchiveMember >> lastModTime [
	"Return my last modification date"

	^ lastModFileDateTime
]

{ #category : 'accessing - extra fields' }
ZipArchiveMember >> localExtraField [
	"Answer the raw extra field of my local header (APPNOTE 4.5)"

	^ localExtraField asByteArray
]

{ #category : 'accessing - extra fields' }
ZipArchiveMember >> localExtraField: aByteArray [
	"Set the raw extra field of my local header (APPNOTE 4.5). Zip64 and extended timestamp blocks are updated when I am written."

	localExtraField := aByteArray asByteArray
]

{ #category : 'private - zip64' }
ZipArchiveMember >> localExtraFieldToWrite [
	"With Zip64, the local header has both sizes in a Zip64 extended information extra field"

	^ self extraFieldToWriteFrom: localExtraField zip64Values: (self usesZip64
			   ifTrue: [
					   {
						   uncompressedSize.
						   self compressedSizeToWrite } ]
			   ifFalse: [ #(  ) ])
]

{ #category : 'accessing' }
ZipArchiveMember >> localFileName [
	"Answer my fileName in terms of the local directory naming convention"
	^ fileName copyReplaceAll: '/' with: FileSystem disk delimiter asString
]

{ #category : 'testing' }
ZipArchiveMember >> looksLikeDirectory [
	^false
]

{ #category : 'private' }
ZipArchiveMember >> mapPermissionsFromUnix: unixPerms [
	"Take Unix permissions (e.g. 8r40755 for a directory) and put them in the high 16 bits. The low 16 bits are for DOS permissions"

	^ unixPerms bitShift: 16
]

{ #category : 'private' }
ZipArchiveMember >> mapPermissionsToUnix: zipPerms [
	"Extract Unix permissions (the high 16 bits) from zip permissions. The low 16 bits are for DOS permissions"

	^ zipPerms bitShift: -16
]

{ #category : 'private - zip64' }
ZipArchiveMember >> mayExceedZip64Limit [
	"Answer whether my sizes may not fit in 4 bytes. Deflate may expand incompressible data a little, so stay away from the limit."

	^ uncompressedSize >= (Zip64Limit - (Zip64Limit // 100))
]

{ #category : 'accessing' }
ZipArchiveMember >> modifiedAt: aDateAndTime [

	lastModFileDateTime := aDateAndTime
]

{ #category : 'private' }
ZipArchiveMember >> readRawChunk: n [
	self subclassResponsibility
]

{ #category : 'private' }
ZipArchiveMember >> rewindData [
	readDataRemaining :=  (desiredCompressionMethod = CompressionDeflated
		and: [ compressionMethod = CompressionDeflated ])
			ifTrue: [ compressedSize ]
			ifFalse: [ uncompressedSize ]
]

{ #category : 'accessing' }
ZipArchiveMember >> splitFileName [
	"Answer my name split on slash boundaries. A directory will have a trailing empty string."
	^ fileName findTokens: '/'
]

{ #category : 'accessing' }
ZipArchiveMember >> uncompressedSize [
	"Return the uncompressed size for this member."
	^uncompressedSize
]

{ #category : 'private - extra fields' }
ZipArchiveMember >> unicodePathIn: aByteArrayOrString forNameBytes: aByteArray [
	"Answer the UTF-8 file name of an Info-ZIP Unicode path block ('up', 0x7075), or nil.
	It holds a version (1), the CRC-32 of the file name of the header, then the UTF-8 name. It is ignored when the CRC does not match, i.e. when the name of the header was changed by a tool that does not know this block."

	| data crc |
	data := self extraFieldIn: aByteArrayOrString withId: UnicodePathExtraFieldId.
	(data isNil or: [ data size < 5 or: [ (data at: 1) ~= 1 ] ]) ifTrue: [ ^ nil ].
	crc := (5 to: 2 by: -1) inject: 0 into: [ :value :index | (value bitShift: 8) + (data at: index) ].
	crc = (CRC crc32FromCollection: aByteArray asByteArray) ifFalse: [ ^ nil ].
	^ [ (data allButFirst: 5) utf8Decoded ]
		  on: ZnCharacterEncodingError
		  do: [ nil ]
]

{ #category : 'accessing' }
ZipArchiveMember >> unixFileAttributes [
	"See comment at end"

	^self mapPermissionsToUnix: externalFileAttributes

"Per http://unix.stackexchange.com/questions/14705/the-zip-formats-external-file-attribute : These values can all be found in <sys/stat.h> - link to 4.4BSD version. These are not in the POSIX standard (which defines test macros instead); but originate from AT&T Unix and BSD. (in GNU libc / Linux, the values themselves are defined as __S_IFDIR etc in bits/stat.h, though the kernel header might be easier to read - the values are all the same pretty much everywhere.)

#define S_IFIFO  0010000  /* named pipe (fifo) */
#define S_IFCHR  0020000  /* character special */
#define S_IFDIR  0040000  /* directory */
#define S_IFBLK  0060000  /* block special */
#define S_IFREG  0100000  /* regular */
#define S_IFLNK  0120000  /* symbolic link */
#define S_IFSOCK 0140000  /* socket */
And of course, the other 12 bits are for the permissions and setuid/setgid/sticky bits, the same as for chmod:

#define S_ISUID 0004000 /* set user id on execution */
#define S_ISGID 0002000 /* set group id on execution */
#define S_ISTXT 0001000 /* sticky bit */
#define S_IRWXU 0000700 /* RWX mask for owner */
#define S_IRUSR 0000400 /* R for owner */
#define S_IWUSR 0000200 /* W for owner */
#define S_IXUSR 0000100 /* X for owner */
#define S_IRWXG 0000070 /* RWX mask for group */
#define S_IRGRP 0000040 /* R for group */
#define S_IWGRP 0000020 /* W for group */
#define S_IXGRP 0000010 /* X for group */
#define S_IRWXO 0000007 /* RWX mask for other */
#define S_IROTH 0000004 /* R for other */
#define S_IWOTH 0000002 /* W for other */
#define S_IXOTH 0000001 /* X for other */
#define S_ISVTX 0001000 /* save swapped text even after use"
]

{ #category : 'accessing' }
ZipArchiveMember >> unixFileAttributes: perms [
	"See #unixFileAttributes comment"

	| oldPerms newPerms |
	oldPerms := self mapPermissionsToUnix: externalFileAttributes.
	newPerms :=  self isDirectory
			ifTrue: [ (perms bitAnd: FileAttrib bitInvert) bitOr: DirectoryAttrib ]
			ifFalse: [ (perms bitAnd: DirectoryAttrib bitInvert) bitOr: FileAttrib ].
	externalFileAttributes := self mapPermissionsFromUnix: newPerms.
	^oldPerms
]

{ #category : 'accessing - extra fields' }
ZipArchiveMember >> unixGid [
	"Answer the Unix group id of an Info-ZIP Unix extra field, or nil"

	^ self unixOwner ifNotNil: [ :owner | owner second ]
]

{ #category : 'accessing - extra fields' }
ZipArchiveMember >> unixOwner [

	^ (self unixOwnerIn: cdExtraField) ifNil: [ self unixOwnerIn: localExtraField ]
]

{ #category : 'private - extra fields' }
ZipArchiveMember >> unixOwnerIn: aByteArrayOrString [
	"Answer the UID and GID of an Info-ZIP Unix block ('ux', 0x7875), or nil.
	It holds a version (1), then the size and value of the UID, then the size and value of the GID, little endian."

	| data uidSize gidSize littleEndian |
	data := self extraFieldIn: aByteArrayOrString withId: UnixOwnerExtraFieldId.
	(data isNil or: [ data size < 3 or: [ (data at: 1) ~= 1 ] ]) ifTrue: [ ^ nil ].
	uidSize := data at: 2.
	data size < (3 + uidSize) ifTrue: [ ^ nil ].
	gidSize := data at: 3 + uidSize.
	data size < (3 + uidSize + gidSize) ifTrue: [ ^ nil ].
	littleEndian := [ :start :size |
	                (start + size - 1 to: start by: -1) inject: 0 into: [ :value :index | (value bitShift: 8) + (data at: index) ] ].
	^ {
		  (littleEndian value: 3 value: uidSize).
		  (littleEndian value: 4 + uidSize value: gidSize) }
]

{ #category : 'accessing - extra fields' }
ZipArchiveMember >> unixUid [
	"Answer the Unix user id of an Info-ZIP Unix extra field, or nil"

	^ self unixOwner ifNotNil: [ :owner | owner first ]
]

{ #category : 'private - zip64' }
ZipArchiveMember >> usesZip64 [
	"Answer whether I was last written with Zip64 extended information (APPNOTE 4.5.3)"

	^ usesZip64 == true
]

{ #category : 'accessing - spec fields' }
ZipArchiveMember >> versionMadeBy [
	"Per .ZIP File Format Specification Version: 6.3.2:
		2 Bytes:
		- The upper byte indicates the compatibility of the file attribute information.  If the external file attributes are compatible with MS-DOS and can be read by PKZIP for DOS version 2.04g then this value will be zero.
		- The lower byte indicates the ZIP specification version (the version of PKWARE Appnote) supported by the software used to encode the file.  The value / 10 indicates the major version number, and the value mod 10 is the minor version number"

	^ versionMadeBy
]

{ #category : 'accessing - spec fields' }
ZipArchiveMember >> versionMadeBy: anInteger [
	"See comment for #versionMadeBy"

	versionMadeBy := anInteger
]

{ #category : 'accessing - spec fields' }
ZipArchiveMember >> versionNeededToExtract [
	"See .ZIP File Format Specification Version: 6.3.2.
		For example, 2.0 means:
		- File is a folder (directory)
		- File is compressed using Deflate compression
		- File is encrypted using traditional PKWARE encryption"

	^ versionNeededToExtract
]

{ #category : 'accessing - spec fields' }
ZipArchiveMember >> versionNeededToExtract: anInteger [
	"See comment for #versionNeededToExtract"

	versionNeededToExtract := anInteger
]

{ #category : 'private - zip64' }
ZipArchiveMember >> versionNeededToWrite [

	^ self usesZip64
		  ifTrue: [ versionNeededToExtract max: Zip64VersionNeededToExtract ]
		  ifFalse: [ versionNeededToExtract ]
]

{ #category : 'private - writing' }
ZipArchiveMember >> writeCentralDirectoryFileHeaderTo: aStream [
	"C2 v3 V4 v5 V2"

	| systemFileName systemFileComment systemCdExtraField endianStream |
	systemFileName := fileName asVmPathName.
	systemFileComment := fileComment utf8Encoded.
	systemCdExtraField := self centralDirectoryExtraFieldToWrite.
	aStream nextPutAll: CentralDirectoryFileHeaderSignature.


	endianStream := ZnEndianessReadWriteStream on: aStream.
	endianStream nextLittleEndianNumber: 1 put: (self versionMadeBy max: self versionNeededToWrite).
	endianStream nextLittleEndianNumber: 1 put: fileAttributeFormat.

	endianStream nextLittleEndianNumber: 2 put: self versionNeededToWrite.
	endianStream nextLittleEndianNumber: 2 put: self bitFlagToWrite.
	endianStream nextLittleEndianNumber: 2 put: desiredCompressionMethod.

	endianStream nextLittleEndianNumber: 4 put: lastModFileDateTime asDosTimestamp.

	"These next 3 should have been updated during the write of the data"
	endianStream nextLittleEndianNumber: 4 put: crc32.
	endianStream nextLittleEndianNumber: 4 put: (self usesZip64
			 ifTrue: [ Zip64Limit ]
			 ifFalse: [ self compressedSizeToWrite ]).
	endianStream nextLittleEndianNumber: 4 put: (self usesZip64
			 ifTrue: [ Zip64Limit ]
			 ifFalse: [ uncompressedSize ]).

	endianStream nextLittleEndianNumber: 2 put: systemFileName size.
	endianStream nextLittleEndianNumber: 2 put: systemCdExtraField size.
	endianStream nextLittleEndianNumber: 2 put: systemFileComment size.
	endianStream nextLittleEndianNumber: 2 put: 0. "diskNumberStart"
	endianStream nextLittleEndianNumber: 2 put: internalFileAttributes.

	endianStream nextLittleEndianNumber: 4 put: externalFileAttributes.
	endianStream nextLittleEndianNumber: 4 put: (self usesZip64
			 ifTrue: [ Zip64Limit ]
			 ifFalse: [ headerStartPosition ]).

	aStream nextPutAll: systemFileName asByteArray.
	aStream nextPutAll: systemCdExtraField asByteArray.
	aStream nextPutAll: systemFileComment
]

{ #category : 'private - writing' }
ZipArchiveMember >> writeDataDescriptorTo: aStream [
	"This writes a data descriptor to the given stream.
	Assumes that crc32, writeOffset, and uncompressedSize are
	set correctly (they should be after a write).
	Further, the local file header should have the
	GPBF:=HAS:=DATA:=DESCRIPTOR:=MASK (8) bit set."

	aStream nextLittleEndianNumber: 4 put: crc32.
	aStream nextLittleEndianNumber: 4 put: compressedSize.
	aStream nextLittleEndianNumber: 4 put: uncompressedSize
]

{ #category : 'private - writing' }
ZipArchiveMember >> writeDataTo: aStream [
	"Copy my (possibly inflated or deflated) data to the given stream.
	This might do compression, decompression, or straight copying, depending
	on the values of compressionMethod and desiredCompressionMethod"

	"Note: Do not shortcut this method if uncompressedSize = 0. Even in this case
	data may be produced by the compressor (i.e., '' zipped size > 0) and must
	be stored in the file or else other utilities will treat the zip file as corrupt."

	(compressionMethod = CompressionStored and: [ desiredCompressionMethod = CompressionDeflated ])
		ifTrue: [ ^self compressDataTo: aStream ].

	(compressionMethod = CompressionDeflated and: [ desiredCompressionMethod = CompressionStored ])
		ifTrue: [ ^self uncompressDataTo: aStream ].

	self copyDataTo: aStream
]

{ #category : 'private - writing' }
ZipArchiveMember >> writeDataTo: aStream from: start to: finish [
	"Copy my (possibly inflated or deflated) data to the given stream.
	But only the specified byte range.
	This might do decompression, or straight copying, depending
	on the values of compressionMethod and desiredCompressionMethod"

	uncompressedSize = 0 ifTrue: [ ^self ].	"nothing to do because no data"
	start > finish ifTrue: [ ^self ].
	start > uncompressedSize ifTrue: [ ^self ].

	(compressionMethod = CompressionStored and: [ desiredCompressionMethod = CompressionDeflated ])
		ifTrue: [ ^self error: 'only supports uncompression or copying right now' ].

	(compressionMethod = CompressionDeflated and: [ desiredCompressionMethod = CompressionStored ])
		ifTrue: [ ^self uncompressDataTo: aStream from: start to: finish ].

	self copyRawDataTo: aStream from: start to: finish
]

{ #category : 'private - writing' }
ZipArchiveMember >> writeLocalFileHeaderTo: aStream [
	"Write my local header to a file handle.
	Stores the offset to the start of the header in my
	writeLocalHeaderRelativeOffset member."

	| systemFileName endianStream extraField |
	systemFileName := fileName asVmPathName.
	extraField := self localExtraFieldToWrite.
	aStream nextPutAll: LocalFileHeaderSignature.

	endianStream := ZnEndianessReadWriteStream on: aStream.
	endianStream nextLittleEndianNumber: 2 put: self versionNeededToWrite.
	endianStream nextLittleEndianNumber: 2 put: self bitFlagToWrite.
	endianStream nextLittleEndianNumber: 2 put: desiredCompressionMethod.

	endianStream nextLittleEndianNumber: 4 put: lastModFileDateTime asDosTimestamp.
	endianStream nextLittleEndianNumber: 4 put: crc32.
	endianStream nextLittleEndianNumber: 4 put: (self usesZip64
			 ifTrue: [ Zip64Limit ]
			 ifFalse: [ self compressedSizeToWrite ]).
	endianStream nextLittleEndianNumber: 4 put: (self usesZip64
			 ifTrue: [ Zip64Limit ]
			 ifFalse: [ uncompressedSize ]).

	endianStream nextLittleEndianNumber: 2 put: systemFileName size.
	endianStream nextLittleEndianNumber: 2 put: extraField size.

	aStream nextPutAll: systemFileName asByteArray.
	aStream nextPutAll: extraField
]

{ #category : 'writing' }
ZipArchiveMember >> writeTo: aStream [

	self writeTo: aStream forcingZip64: false
]

{ #category : 'writing' }
ZipArchiveMember >> writeTo: aStream forcingZip64: aBoolean [
	"The header contains information about the data, including compressed size and CRC,
	 that we do not know yet! So we leave room for the header, write the data, and go
	 back to write the header after we have the needed information.
	Zip64 extended information is written when aBoolean is true, or when my sizes or position may not fit in 4 bytes."

	| headerSize headerEndPosition dataEndPosition |
	self rewindData.
	headerStartPosition := aStream position.
	usesZip64 := aBoolean or: [ self mayExceedZip64Limit or: [ headerStartPosition >= Zip64Limit ] ].
	headerSize := 30 + fileName asVmPathName asByteArray size + self localExtraFieldToWrite size.
	headerEndPosition := headerStartPosition + headerSize.
	aStream nextPutAll: (ByteArray new: headerSize).
	self writeDataTo: aStream.
	dataEndPosition := aStream position.
	(self usesZip64 not and: [ uncompressedSize >= Zip64Limit or: [ self compressedSizeToWrite >= Zip64Limit ] ]) ifTrue: [
		ZipArchiveError signal: fileName printString , ' is too large for a header without Zip64 extended information' ].
	aStream position: headerStartPosition.
	self writeLocalFileHeaderTo: aStream.
	"Verify that our calculation of the header size was correct!"
	aStream position == headerEndPosition ifFalse: [ self error: 'Incorrect header size!' ].
	aStream position: dataEndPosition
]

{ #category : 'accessing - extra fields' }
ZipArchiveMember >> writesExtendedTimestamp: aBoolean [
	"When true, my modification time is also written in extended timestamp extra fields ('UT'), in UTC with a precision of one second"

	writesExtendedTimestamp := aBoolean
]

{ #category : 'private - zip64' }
ZipArchiveMember >> zip64DataFor: aCollectionOfIntegers [
	"Answer the data of a Zip64 extended information extra field holding the given values as 8 bytes little endian integers, or nil if there is none"

	aCollectionOfIntegers ifEmpty: [ ^ nil ].
	^ ByteArray streamContents: [ :stream |
		  aCollectionOfIntegers do: [ :each |
			  0 to: 7 do: [ :byteIndex | stream nextPut: ((each bitShift: byteIndex * -8) bitAnd: 255) ] ] ]
]

{ #category : 'private - zip64' }
ZipArchiveMember >> zip64ValuesIn: aByteArrayOrString [
	"Answer a stream on the 8 bytes little endian values of the Zip64 extended information extra field (APPNOTE 4.5.3), empty if there is none"

	| data |
	data := (self extraFieldIn: aByteArrayOrString withId: Zip64ExtraFieldId) ifNil: [ #[  ] ].
	^ ((0 to: data size // 8 - 1) collect: [ :index |
		   (8 to: 1 by: -1) inject: 0 into: [ :value :byteIndex | (value bitShift: 8) + (data at: index * 8 + byteIndex) ] ]) readStream
]
