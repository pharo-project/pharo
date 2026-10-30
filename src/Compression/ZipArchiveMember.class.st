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
		'writesExtendedTimestamp',
		'password'
	],
	#pools : [
		'ZipFileConstants'
	],
	#category : 'Compression-Archives',
	#package : 'Compression',
	#tag : 'Archives'
}

{ #category : 'compression methods' }
ZipArchiveMember class >> compressionMethodNameFor: anInteger [

	^ self compressionMethodNames at: anInteger ifAbsent: [ 'unknown' ]
]

{ #category : 'compression methods' }
ZipArchiveMember class >> compressionMethodNames [
	"Compression methods of APPNOTE 4.4.5"

	^ Dictionary newFrom: {
			  (0 -> 'stored').
			  (1 -> 'shrunk').
			  (2 -> 'reduced (factor 1)').
			  (3 -> 'reduced (factor 2)').
			  (4 -> 'reduced (factor 3)').
			  (5 -> 'reduced (factor 4)').
			  (6 -> 'imploded').
			  (7 -> 'tokenized').
			  (8 -> 'deflated').
			  (9 -> 'Deflate64').
			  (10 -> 'PKWARE DCL imploded').
			  (12 -> 'bzip2').
			  (14 -> 'LZMA').
			  (16 -> 'IBM z/OS CMPSC').
			  (18 -> 'IBM TERSE').
			  (19 -> 'IBM LZ77 z').
			  (20 -> 'Zstandard (deprecated id)').
			  (93 -> 'Zstandard').
			  (94 -> 'MP3').
			  (95 -> 'XZ').
			  (96 -> 'JPEG variant').
			  (97 -> 'WavPack').
			  (98 -> 'PPMd').
			  (99 -> 'AE-x encryption') }
]

{ #category : 'accessing' }
ZipArchiveMember class >> cp437HighCodePoints [
	"Unicode code points of the bytes 128 to 255 of IBM code page 437, the encoding of ZIP file names and comments when the language encoding flag is not set (APPNOTE, appendix D)"

	^ #( 199 252 233 226 228 224 229 231 234 235 232 239 238 236 196 197 201 230 198 244 246 242 251 249 255 214 220 162 163 165 8359
	     402 225 237 243 250 241 209 170 186 191 8976 172 189 188 161 171 187 9617 9618 9619 9474 9508 9569 9570 9558 9557 9571 9553
	     9559 9565 9564 9563 9488 9492 9524 9516 9500 9472 9532 9566 9567 9562 9556 9577 9574 9568 9552 9580 9575 9576 9572 9573 9561
	     9560 9554 9555 9579 9578 9496 9484 9608 9604 9612 9616 9600 945 223 915 960 931 963 181 964 934 920 937 948 8734 966 949 8745
	     8801 177 8805 8804 8992 8993 247 8776 176 8729 183 8730 8319 178 9632 160 )
]

{ #category : 'decoding' }
ZipArchiveMember class >> decodeCp437: aByteArray [

	| highCodePoints |
	highCodePoints := self cp437HighCodePoints.
	^ String streamContents: [ :stream |
			  aByteArray do: [ :each |
					  stream nextPut: (Character value: (each < 128
								    ifTrue: [ each ]
								    ifFalse: [ highCodePoints at: each - 127 ])) ] ]
]

{ #category : 'decoding' }
ZipArchiveMember class >> decodeText: aByteArray [
	"Decode text read from an archive without the language encoding flag (APPNOTE, appendix D).
	It should be CP437, but many tools (and older versions of Pharo) write UTF-8, so valid UTF-8 is decoded as UTF-8."

	| bytes |
	bytes := aByteArray asByteArray.
	^ [ bytes utf8Decoded ]
		  on: ZnCharacterEncodingError
		  do: [ self decodeCp437: bytes ]
]

{ #category : 'compression methods' }
ZipArchiveMember class >> decompressorForMethod: anInteger [
	"Answer a block decompressing a ByteArray compressed with the compression method anInteger, or nil.
	Stored and deflated data are handled by the members themselves. Other compression methods can be supported by a class side method taking the compressed ByteArray and answering the decompressed one, with the pragma <zipDecompressorForMethod: anInteger>."

	^ (Pragma allNamed: #zipDecompressorForMethod:)
		  detect: [ :pragma | (pragma argumentAt: 1) = anInteger ]
		  ifFound: [ :pragma |
				  | receiver |
				  receiver := pragma methodClass instanceSide.
				  [ :bytes | receiver perform: pragma method selector with: bytes ] ]
		  ifNone: [ nil ]
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

{ #category : 'initialization' }
ZipArchiveMember >> beSymbolicLink [
	"Mark me as a Unix symbolic link, with permissions rwxrwxrwx. My contents must be the link target."

	fileAttributeFormat := FaUnix.
	externalFileAttributes := self mapPermissionsFromUnix: (SymbolicLinkAttrib bitOr: 8r777)
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

{ #category : 'testing' }
ZipArchiveMember >> canDecompress [
	"Answer whether my data can be extracted: it is stored, deflated, or a decompressor is registered for its compression method"

	^ compressionMethod = CompressionStored or: [
		  compressionMethod = CompressionDeflated or: [ (self class decompressorForMethod: compressionMethod) isNotNil ] ]
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

{ #category : 'private - encryption' }
ZipArchiveMember >> checkCanDecrypt [

	self isStronglyEncrypted ifTrue: [
			ZipArchiveError signal:
				'member ' , fileName , ' uses AES or strong encryption, which is not supported (only traditional ZipCrypto is)' ].
	password ifNil: [ ZipPasswordError signal: 'member ' , fileName , ' is encrypted; set its password with #password:' ]
]

{ #category : 'private - writing' }
ZipArchiveMember >> checkCrc: aCrc [
	"Signal a CRCError, and mark me as corrupt, when aCrc is not my CRC-32"

	aCrc = crc32 ifTrue: [ ^ self ].
	self isCorrupt: true.
	CRCError signal: ('Wrong CRC-32 in {1} (expected {2} got {3})' format: {
				 fileName printString.
				 crc32 printStringHex.
				 aCrc printStringHex })
]

{ #category : 'private - writing' }
ZipArchiveMember >> checkCrcOf: aByteArray [
	"Signal a CRCError, and mark me as corrupt, when aByteArray does not match my CRC-32"

	self checkCrc: (CRC crc32FromCollection: aByteArray)
]

{ #category : 'accessing' }
ZipArchiveMember >> clearExtraFields [
	cdExtraField := ''.
	localExtraField := ''
]

{ #category : 'private - writing' }
ZipArchiveMember >> compressDataTo: aStream [
	"Copy my deflated data to the given stream, compressed with my desiredCompressionLevel."

	| encoder startPos |
	encoder := ZipWriteStream on: aStream level: self deflateLevel.
	startPos := aStream position.
	self readRawChunksDo: [ :chunk | encoder nextPutAll: chunk asByteArray ].
	encoder finish. "not close!"
	compressedSize := aStream position - startPos.
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
	"The compressed size written in my headers. Stored data is not compressed,
	but when encrypted it is preceded by the encryption header."

	^ (desiredCompressionMethod = CompressionStored and: [ self isEncrypted not ])
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

{ #category : 'testing' }
ZipArchiveMember >> compressionMethodName [
	"Answer the name of my compression method, as in APPNOTE 4.4.5"

	^ self class compressionMethodNameFor: compressionMethod
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

{ #category : 'initialization' }
ZipArchiveMember >> copyAttributesFrom: aMember [
	"Take the comment, file attributes and compression method of aMember, which I replace. My data, CRC-32, sizes and modification time are my own."

	fileComment := aMember fileComment.
	fileAttributeFormat := aMember fileAttributeFormat.
	internalFileAttributes := aMember internalFileAttributes.
	externalFileAttributes := aMember externalFileAttributes.
	self desiredCompressionMethod: aMember desiredCompressionMethod
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
	crc32 := self copyRawDataComputingCrcTo: aStream
]

{ #category : 'private - encryption' }
ZipArchiveMember >> copyEncryptedDataTo: aStream [
	"Copy my encrypted data, and its data descriptor if I have one, since the
	encryption check byte depends on it"

	desiredCompressionMethod := compressionMethod.
	readDataRemaining := compressedSize.
	self copyRawDataTo: aStream.
	self hasDataDescriptor ifTrue: [ self writeDataDescriptorTo: aStream ]
]

{ #category : 'private - writing' }
ZipArchiveMember >> copyRawDataComputingCrcTo: aStream [
	"Copy my remaining raw data to aStream and answer its CRC-32"

	| crc |
	crc := 16rFFFFFFFF.
	self readRawChunksDo: [ :chunk |
			aStream nextPutAll: chunk.
			crc := CRC
				       update: crc
				       from: 1
				       to: chunk size
				       in: chunk ].
	^ crc bitXor: 16rFFFFFFFF
]

{ #category : 'private - writing' }
ZipArchiveMember >> copyRawDataTo: aStream [

	self readRawChunksDo: [ :chunk | aStream nextPutAll: chunk ]
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

{ #category : 'private - reading' }
ZipArchiveMember >> decodeText: aByteArray [
	"Decode a file name or comment read from the archive (APPNOTE, appendix D):
	UTF-8 with the language encoding flag, see the class side otherwise"

	(self bitFlag anyMask: LanguageEncodingFlag) ifTrue: [ ^ aByteArray asByteArray utf8Decoded ].
	^ self class decodeText: aByteArray
]

{ #category : 'private - writing' }
ZipArchiveMember >> decompress: aByteArray [
	"Answer my data decompressed from aByteArray, my compressed data. Besides stored
	and deflated data, the decompressors registered with #decompressorForMethod: are used."

	| decompressor |
	compressionMethod = CompressionStored ifTrue: [ ^ aByteArray ].
	compressionMethod = CompressionDeflated ifTrue: [ ^ (ZipReadStream on: aByteArray readStream) next: uncompressedSize ].
	decompressor := self class decompressorForMethod: compressionMethod.
	decompressor ifNil: [ ^ self signalUnsupportedCompressionMethod ].
	^ (decompressor value: aByteArray) asByteArray
]

{ #category : 'private - writing' }
ZipArchiveMember >> decompressDataTo: aStream [
	"Decompress data that is neither stored nor deflated, when a decompressor is registered for its compression method (see #decompressorForMethod:)"

	| data |
	self canDecompress ifFalse: [ ^ self signalUnsupportedCompressionMethod ].
	readDataRemaining := compressedSize.
	data := self decompress: self readRawData.
	self checkCrcOf: data.
	aStream nextPutAll: data
]

{ #category : 'private - encryption' }
ZipArchiveMember >> decryptedContents [
	"Answer my uncompressed data, decrypted with my password. The CRC is checked."

	| contents |
	contents := self decompress: self decryptedData.
	self checkCrcOf: contents.
	^ contents
]

{ #category : 'private - encryption' }
ZipArchiveMember >> decryptedData [
	"Answer my data, still compressed, decrypted with my password"

	| raw plain headerSize |
	self checkCanDecrypt.
	headerSize := ZipCrypto headerSize.
	self rewindData.
	readDataRemaining := compressedSize.
	raw := self readRawData.
	raw size < headerSize ifTrue: [ ZipArchiveError signal: 'encrypted member ' , fileName , ' is truncated' ].
	plain := (ZipCrypto password: password) decrypt: raw.
	(plain at: headerSize) = self encryptionCheckByte ifFalse: [ ZipPasswordError signal: 'wrong password for member ' , fileName ].
	^ plain allButFirst: headerSize
]

{ #category : 'private - writing' }
ZipArchiveMember >> deflateLevel [
	"The level used to deflate my data: my desiredCompressionLevel, or the default one when it is not a level from 1 to 9"

	^ (desiredCompressionLevel isInteger and: [ desiredCompressionLevel between: 1 and: 9 ])
		  ifTrue: [ desiredCompressionLevel ]
		  ifFalse: [ CompressionLevelDefault ]
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
	"Set the compression method that will be used to write me, and answer the previous one.
	Stored and deflated data can always be written. Data read with another method can only
	be copied as is, or extracted (stored) when a decompressor is registered for it
	(see #decompressorForMethod:); #writeDataTo: signals a ZipArchiveError otherwise.
	Changing to CompressionStored will change my desiredCompressionLevel
	to CompressionLevelNone; changing to CompressionDeflated will change my
	desiredCompressionLevel to CompressionLevelDefault.
	The compressed size of an encrypted stored member includes its encryption header."

	| old |
	old := desiredCompressionMethod.
	desiredCompressionMethod := aNumber.
	desiredCompressionLevel := aNumber = CompressionDeflated
		                           ifTrue: [ CompressionLevelDefault ]
		                           ifFalse: [ CompressionLevelNone ].
	(compressionMethod = CompressionStored and: [ self isEncrypted not ]) ifTrue: [ compressedSize := uncompressedSize ].
	^ old
]

{ #category : 'private - encryption' }
ZipArchiveMember >> encryptionCheckByte [
	"The last byte of the encryption header: the high byte of the DOS modification
	time when I have a data descriptor, else the high byte of my CRC-32"

	^ self hasDataDescriptor
		  ifTrue: [ (lastModFileDateTime asDosTimestamp bitShift: -8) bitAnd: 255 ]
		  ifFalse: [ (crc32 bitShift: -24) bitAnd: 255 ]
]

{ #category : 'private' }
ZipArchiveMember >> endRead [
	readDataRemaining := 0
]

{ #category : 'private - extraction' }
ZipArchiveMember >> ensureExtractionTarget: aFileReference isInside: aDirectory [
	"Zip slip protection: refuse to extract me outside of aDirectory, whatever my file name contains ('..', absolute paths, ...).
	A directory member may be extracted to aDirectory itself."

	| target directory |
	target := aFileReference asFileReference canonicalize.
	directory := aDirectory asFileReference canonicalize.
	((directory contains: target) or: [ self isDirectory and: [ target = directory ] ]) ifFalse: [
		ZipArchiveError signal: 'Refusing to extract ' , fileName printString , ' outside of ' , directory fullName ]
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

{ #category : 'accessing - spec fields' }
ZipArchiveMember >> externalFileAttributes [

	^ externalFileAttributes
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
	(writesExtendedTimestamp or: [ (self extraFieldIn: extraField withId: ExtendedTimestampExtraFieldId) isNotNil ]) ifTrue: [
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

{ #category : 'extraction' }
ZipArchiveMember >> extractInDirectory: aDirectory [
	"Extract this entry into the given directory, keeping an existing file"

	^ self extractInDirectory: aDirectory overwrite: false
]

{ #category : 'extraction' }
ZipArchiveMember >> extractInDirectory: aDirectory overwrite: aBoolean [
	"Extract me under aDirectory, at the path given by my file name, creating the
	directories it needs. An existing file is kept unless aBoolean is true.
	Refuse to extract me outside of aDirectory (zip slip)."

	| file |
	file := self fileInDirectory: aDirectory.
	self ensureExtractionTarget: file isInside: aDirectory.
	self extractToFile: file overwrite: aBoolean
]

{ #category : 'extraction' }
ZipArchiveMember >> extractInDirectory: dir withoutInformingOverwrite: overwrite [
	"Extract myself to dir without informing user whether to overwrite existing file"

	self extractInDirectory: dir overwrite: overwrite
]

{ #category : 'extraction' }
ZipArchiveMember >> extractTo: aStream [

	| oldCompression |
	self hasEncryptedData ifTrue: [ ^ aStream nextPutAll: self decryptedContents ].
	oldCompression := self desiredCompressionMethod: CompressionStored.
	self rewindData.
	self writeDataTo: aStream.
	self desiredCompressionMethod: oldCompression.
	self endRead
]

{ #category : 'extraction' }
ZipArchiveMember >> extractTo: aStream from: start to: finish [

	| oldCompression |
	self hasEncryptedData ifTrue: [
			| contents |
			contents := self decryptedContents copyFrom: (start max: 1) to: (finish min: uncompressedSize).
			^ aStream nextPutAll: (aStream isBinary
					   ifTrue: [ contents ]
					   ifFalse: [ contents asString ]) ].
	aStream binary.
	oldCompression := self desiredCompressionMethod: CompressionStored.
	self rewindData.
	self writeDataTo: aStream from: start to: finish.
	self desiredCompressionMethod: oldCompression.
	self endRead
]

{ #category : 'private - extraction' }
ZipArchiveMember >> extractToFile: aFile overwrite: aBoolean [
	"Extract this entry into aFile, creating the directories it needs. An existing file is kept unless aBoolean is true.
	Nothing is written when my compression method is not supported.
	aFile is trusted: callers check it first when it is built from my file name (zip slip)."

	self isDirectory ifTrue: [ ^ aFile ensureCreateDirectory ].
	aFile parent ensureCreateDirectory.
	aFile exists ifTrue: [
			aBoolean ifFalse: [ ^ self ].
			aFile ensureDelete ].
	self canDecompress ifFalse: [ ^ self signalUnsupportedCompressionMethod ].
	aFile binaryWriteStreamDo: [ :stream | self extractTo: stream ]
]

{ #category : 'extraction' }
ZipArchiveMember >> extractToFileNamed: aFileName [
	"Extract me to aFileName (a String or a FileReference), resolved against the working directory, replacing an existing file.
	The caller chose aFileName, so it is trusted."

	self extractToFileNamed: aFileName inDirectory: FileSystem workingDirectory overwrite: true
]

{ #category : 'extraction' }
ZipArchiveMember >> extractToFileNamed: aLocalFileName inDirectory: dir [
	self extractToFileNamed: aLocalFileName inDirectory: dir overwrite: true
]

{ #category : 'extraction' }
ZipArchiveMember >> extractToFileNamed: aFileName inDirectory: dir overwrite: overwrite [
	"Extract me to aFileName (a String or a FileReference), resolved against dir.
	The caller chose aFileName, so it is trusted and may lead outside of dir."

	self extractToFile: (dir resolve: aFileName) canonicalize overwrite: overwrite
]

{ #category : 'extraction' }
ZipArchiveMember >> extractWithOverwriteInDirectory: dir [

	self extractInDirectory: dir overwrite: true
]

{ #category : 'accessing - spec fields' }
ZipArchiveMember >> fileAttributeFormat [
	"Answer the host system of my external file attributes (FaMsdos or FaUnix)"

	^ fileAttributeFormat
]

{ #category : 'accessing' }
ZipArchiveMember >> fileComment [
	^fileComment
]

{ #category : 'accessing' }
ZipArchiveMember >> fileComment: aString [
	fileComment := aString
]

{ #category : 'accessing' }
ZipArchiveMember >> fileInDirectory: aDirectory [
	"Answer the file reference I would be extracted to under aDirectory"

	^ self splitFileName inject: aDirectory into: [ :base :part | base / part ]
]

{ #category : 'private - extra fields' }
ZipArchiveMember >> fileNameFrom: aByteArray extraField: anExtraField [
	"Decode the file name of a header, preferring the UTF-8 name of a matching Unicode path block"

	^ (self unicodePathIn: anExtraField forNameBytes: aByteArray) ifNil: [ self decodeText: aByteArray ]
]

{ #category : 'testing' }
ZipArchiveMember >> hasContainedSymbolicLinkTarget [
	"Answer whether I am a symbolic link whose target stays inside the extraction
	directory: it is relative, and its '..' components never climb above the root
	of the archive, starting from the directory holding me."

	| target depth |
	self isSymbolicLink ifFalse: [ ^ false ].
	target := self symbolicLinkTarget.
	(target isEmpty or: [ target first = $/ or: [ target first = $\ or: [ target size > 1 and: [ (target at: 2) = $: ] ] ] ])
		ifTrue: [ ^ false ].
	depth := self splitFileName size - 1.
	(target findTokens: '/\') do: [ :each |
			each = '..'
				ifTrue: [
						depth := depth - 1.
						depth < 0 ifTrue: [ ^ false ] ]
				ifFalse: [ each = '.' ifFalse: [ depth := depth + 1 ] ] ].
	^ true
]

{ #category : 'testing' }
ZipArchiveMember >> hasDataDescriptor [
	"Answer whether my CRC-32 and sizes follow my data, in a data descriptor (bit 3)"

	^ (self bitFlag bitAnd: DataDescriptorFlag) ~= 0
]

{ #category : 'testing' }
ZipArchiveMember >> hasEncryptedData [
	"Answer whether the data I read from is encrypted.
	Only members read from an archive can hold encrypted data."

	^ false
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
	usesZip64 := false.
	writesExtendedTimestamp := false.
	self unixFileAttributes: DefaultFilePermissions
]

{ #category : 'accessing - spec fields' }
ZipArchiveMember >> internalFileAttributes [

	^ internalFileAttributes
]

{ #category : 'testing' }
ZipArchiveMember >> isDirectory [
	^false
]

{ #category : 'testing' }
ZipArchiveMember >> isEncrypted [
	"Return true if this member is encrypted (this is unsupported)"
	^ (self bitFlag bitAnd: EncryptedFlag) ~= 0
]

{ #category : 'testing' }
ZipArchiveMember >> isStronglyEncrypted [
	"Answer whether I use AES (compression method 99) or PKWARE strong
	encryption (bit 6), neither of which is supported"

	^ self isEncrypted and: [ (self bitFlag bitAnd: StrongEncryptionFlag) ~= 0 or: [ compressionMethod = CompressionAES ] ]
]

{ #category : 'testing' }
ZipArchiveMember >> isSymbolicLink [
	"Answer whether I am a Unix symbolic link: my Unix file type, in the high bits
	of my external attributes, is S_IFLNK. My data is then the link target."

	^ fileAttributeFormat = FaUnix and: [ (self unixFileAttributes bitAnd: UnixFileTypeMask) = SymbolicLinkAttrib ]
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

{ #category : 'accessing' }
ZipArchiveMember >> password [
	"Answer the password used to decrypt or encrypt my data, or nil"

	^ password
]

{ #category : 'accessing' }
ZipArchiveMember >> password: aStringOrByteArray [
	"Set the password used to decrypt my data when I am an encrypted member,
	or to encrypt my data when I am written. Encryption uses the traditional
	PKWARE cipher (ZipCrypto), which is weak: see the comment of ZipCrypto.
	A member read already encrypted is copied as is when written."

	password := aStringOrByteArray
]

{ #category : 'private - encryption' }
ZipArchiveMember >> plainDataToEncrypt [
	"Answer my data as it would be written without encryption, and set my CRC-32 and sizes"

	| buffer |
	(compressionMethod = CompressionStored and: [ desiredCompressionMethod = CompressionStored ]) ifTrue: [
			| data |
			data := self readRawData.
			uncompressedSize := data size.
			crc32 := CRC crc32FromCollection: data.
			^ data ].
	buffer := WriteStream on: (ByteArray new: 4096).
	self writeDataTo: buffer.
	^ buffer contents
]

{ #category : 'private' }
ZipArchiveMember >> readRawChunk: n [
	self subclassResponsibility
]

{ #category : 'private - writing' }
ZipArchiveMember >> readRawChunksDo: aBlock [
	"Evaluate aBlock with my remaining raw data, in chunks"

	[ readDataRemaining > 0 ] whileTrue: [
			| chunk |
			chunk := self readRawChunk: (4096 min: readDataRemaining).
			readDataRemaining := readDataRemaining - chunk size.
			aBlock value: chunk ]
]

{ #category : 'private - writing' }
ZipArchiveMember >> readRawData [
	"Answer all my remaining raw (possibly compressed or encrypted) data"

	^ ByteArray
		  new: readDataRemaining
		  streamContents: [ :stream | self readRawChunksDo: [ :chunk | stream nextPutAll: chunk asByteArray ] ]
]

{ #category : 'private' }
ZipArchiveMember >> rewindData [
	readDataRemaining :=  (desiredCompressionMethod = CompressionDeflated
		and: [ compressionMethod = CompressionDeflated ])
			ifTrue: [ compressedSize ]
			ifFalse: [ uncompressedSize ]
]

{ #category : 'private - writing' }
ZipArchiveMember >> signalUnsupportedCompressionMethod [

	^ ZipArchiveError signal:
		  'compression method ' , compressionMethod printString , ' (' , self compressionMethodName
		  , ') is not supported, cannot extract ' , fileName printString
]

{ #category : 'accessing' }
ZipArchiveMember >> splitFileName [
	"Answer my name split on slash boundaries. A directory will have a trailing empty string."
	^ fileName findTokens: '/'
]

{ #category : 'accessing' }
ZipArchiveMember >> symbolicLinkTarget [
	"Answer the target of the symbolic link I represent, decoded from UTF-8"

	self isSymbolicLink ifFalse: [ ZipArchiveError signal: fileName , ' is not a symbolic link' ].
	^ self contents asByteArray utf8Decoded
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

	^ usesZip64
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
	"Write my central directory header (APPNOTE 4.3.12). My data must have been written,
	so that my CRC-32, sizes and header position are known."

	| systemFileName systemFileComment systemCdExtraField endianStream |
	systemFileName := fileName asVmPathName.
	systemFileComment := fileComment utf8Encoded.
	systemCdExtraField := self centralDirectoryExtraFieldToWrite.
	aStream nextPutAll: CentralDirectoryFileHeaderSignature.
	endianStream := ZnEndianessReadWriteStream on: aStream.
	endianStream nextLittleEndianNumber: 1 put: (self versionMadeBy max: self versionNeededToWrite).
	endianStream nextLittleEndianNumber: 1 put: fileAttributeFormat.
	self writeCommonHeaderFieldsTo: endianStream fileNameSize: systemFileName size extraFieldSize: systemCdExtraField size.
	endianStream nextLittleEndianNumber: 2 put: systemFileComment size.
	endianStream nextLittleEndianNumber: 2 put: 0. "diskNumberStart"
	endianStream nextLittleEndianNumber: 2 put: internalFileAttributes.
	endianStream nextLittleEndianNumber: 4 put: externalFileAttributes.
	endianStream nextLittleEndianNumber: 4 put: (self zip64Masked: headerStartPosition).
	aStream nextPutAll: systemFileName asByteArray.
	aStream nextPutAll: systemCdExtraField asByteArray.
	aStream nextPutAll: systemFileComment
]

{ #category : 'private - writing' }
ZipArchiveMember >> writeCommonHeaderFieldsTo: endianStream fileNameSize: nameSize extraFieldSize: extraSize [
	"Write the fields shared by the local and central directory headers, from 'version
	needed to extract' to 'extra field length' (APPNOTE 4.3.7 and 4.3.12)"

	endianStream nextLittleEndianNumber: 2 put: self versionNeededToWrite.
	endianStream nextLittleEndianNumber: 2 put: self bitFlagToWrite.
	endianStream nextLittleEndianNumber: 2 put: desiredCompressionMethod.
	endianStream nextLittleEndianNumber: 4 put: lastModFileDateTime asDosTimestamp.
	endianStream nextLittleEndianNumber: 4 put: crc32.
	endianStream nextLittleEndianNumber: 4 put: (self zip64Masked: self compressedSizeToWrite).
	endianStream nextLittleEndianNumber: 4 put: (self zip64Masked: uncompressedSize).
	endianStream nextLittleEndianNumber: 2 put: nameSize.
	endianStream nextLittleEndianNumber: 2 put: extraSize
]

{ #category : 'private - writing' }
ZipArchiveMember >> writeDataDescriptorTo: aStream [
	"Write my data descriptor, with its optional signature. Its sizes take 8 bytes
	when I use Zip64 (APPNOTE 4.3.9). Assumes that crc32, compressedSize and
	uncompressedSize are set, and that my bit flag has bit 3 set."

	| endianStream sizeLength |
	sizeLength := self usesZip64
		              ifTrue: [ 8 ]
		              ifFalse: [ 4 ].
	aStream nextPutAll: DataDescriptorSignature.
	endianStream := ZnEndianessReadWriteStream on: aStream.
	endianStream nextLittleEndianNumber: 4 put: crc32.
	endianStream nextLittleEndianNumber: sizeLength put: compressedSize.
	endianStream nextLittleEndianNumber: sizeLength put: uncompressedSize
]

{ #category : 'private - writing' }
ZipArchiveMember >> writeDataTo: aStream [
	"Copy my (possibly inflated or deflated) data to the given stream.
	This might do compression, decompression, or straight copying, depending
	on the values of compressionMethod and desiredCompressionMethod"

	"Note: Do not shortcut this method if uncompressedSize = 0. Even in this case
	data may be produced by the compressor (i.e., '' zipped size > 0) and must
	be stored in the file or else other utilities will treat the zip file as corrupt."

	(compressionMethod = CompressionStored and: [ desiredCompressionMethod = CompressionDeflated ]) ifTrue: [
		^ self compressDataTo: aStream ].

	(compressionMethod = CompressionDeflated and: [ desiredCompressionMethod = CompressionStored ]) ifTrue: [
		^ self uncompressDataTo: aStream ].

	compressionMethod = desiredCompressionMethod ifTrue: [ ^ self copyDataTo: aStream ].

	desiredCompressionMethod = CompressionStored ifTrue: [ ^ self decompressDataTo: aStream ].

	ZipArchiveError signal:
		'cannot write ' , fileName printString , ' with compression method ' , desiredCompressionMethod printString , ' ('
		, (self class compressionMethodNameFor: desiredCompressionMethod) , ')'
]

{ #category : 'private - writing' }
ZipArchiveMember >> writeDataTo: aStream from: start to: finish [
	"Copy my (possibly inflated or deflated) data to the given stream.
	But only the specified byte range.
	This might do decompression, or straight copying, depending
	on the values of compressionMethod and desiredCompressionMethod"

	uncompressedSize = 0 ifTrue: [ ^ self ]. "nothing to do because no data"
	start > finish ifTrue: [ ^ self ].
	start > uncompressedSize ifTrue: [ ^ self ].

	(compressionMethod = CompressionStored and: [ desiredCompressionMethod = CompressionDeflated ]) ifTrue: [
		^ ZipArchiveError signal: 'a range of ' , fileName printString , ' can only be extracted, not compressed' ].

	(compressionMethod = CompressionDeflated and: [ desiredCompressionMethod = CompressionStored ]) ifTrue: [
		^ self uncompressDataTo: aStream from: start to: finish ].

	(compressionMethod ~= CompressionStored and: [ desiredCompressionMethod = CompressionStored ]) ifTrue: [
			| data |
			data := ByteArray streamContents: [ :stream | self decompressDataTo: stream ].
			^ aStream nextPutAll: (data copyFrom: start to: (finish min: data size)) ].

	self copyRawDataTo: aStream from: start to: finish
]

{ #category : 'private - encryption' }
ZipArchiveMember >> writeEncryptedDataTo: aStream [
	"Write my (possibly compressed) data encrypted with ZipCrypto, preceded by
	the 12-byte encryption header. No data descriptor is written, so the check
	byte is the high byte of the CRC-32."

	| data header random crypto |
	self bitFlag: ((self bitFlag bitOr: EncryptedFlag) bitClear: DataDescriptorFlag).
	data := self plainDataToEncrypt.
	random := Random new.
	header := ByteArray new: ZipCrypto headerSize.
	1 to: header size - 1 do: [ :i | header at: i put: (random nextInteger: 256) - 1 ].
	header at: header size put: self encryptionCheckByte.
	crypto := ZipCrypto password: password.
	aStream
		nextPutAll: (crypto encrypt: header);
		nextPutAll: (crypto encrypt: data).
	compressedSize := data size + header size
]

{ #category : 'private - writing' }
ZipArchiveMember >> writeLocalFileHeaderTo: aStream [
	"Write my local header (APPNOTE 4.3.7)"

	| systemFileName extraField |
	systemFileName := fileName asVmPathName.
	extraField := self localExtraFieldToWrite.
	aStream nextPutAll: LocalFileHeaderSignature.
	self
		writeCommonHeaderFieldsTo: (ZnEndianessReadWriteStream on: aStream)
		fileNameSize: systemFileName size
		extraFieldSize: extraField size.
	aStream nextPutAll: systemFileName asByteArray.
	aStream nextPutAll: extraField
]

{ #category : 'private - encryption' }
ZipArchiveMember >> writeMemberDataTo: aStream [
	"Write my data, encrypting it when I have a password.
	Data read already encrypted is copied as is."

	self hasEncryptedData ifTrue: [ ^ self copyEncryptedDataTo: aStream ].
	password ifNil: [
			self bitFlag: (self bitFlag bitClear: EncryptedFlag).
			^ self writeDataTo: aStream ].
	self writeEncryptedDataTo: aStream
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
	self writeMemberDataTo: aStream.
	dataEndPosition := aStream position.
	(self usesZip64 not and: [ uncompressedSize >= Zip64Limit or: [ self compressedSizeToWrite >= Zip64Limit ] ]) ifTrue: [
		ZipArchiveError signal: fileName printString , ' is too large for a header without Zip64 extended information' ].
	aStream position: headerStartPosition.
	self writeLocalFileHeaderTo: aStream.
	"Verify that our calculation of the header size was correct!"
	aStream position == headerEndPosition ifFalse: [ ZipArchiveError signal: 'incorrect header size for ' , fileName printString ].
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

{ #category : 'private - writing' }
ZipArchiveMember >> zip64Masked: anInteger [
	"Answer the value written in a 4-byte header field for anInteger: with Zip64 the field
	holds 16rFFFFFFFF and the value is in the extended information extra field"

	^ self usesZip64
		  ifTrue: [ Zip64Limit ]
		  ifFalse: [ anInteger ]
]

{ #category : 'private - zip64' }
ZipArchiveMember >> zip64ValuesIn: aByteArrayOrString [
	"Answer a stream on the 8 bytes little endian values of the Zip64 extended information extra field (APPNOTE 4.5.3), empty if there is none"

	| data |
	data := (self extraFieldIn: aByteArrayOrString withId: Zip64ExtraFieldId) ifNil: [ #[  ] ].
	^ ((0 to: data size // 8 - 1) collect: [ :index |
		   (8 to: 1 by: -1) inject: 0 into: [ :value :byteIndex | (value bitShift: 8) + (data at: index * 8 + byteIndex) ] ]) readStream
]
