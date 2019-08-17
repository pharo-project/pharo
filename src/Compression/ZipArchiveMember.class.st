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
	#name : #ZipArchiveMember,
	#superclass : #ArchiveMember,
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
		'headerStartPosition'
	],
	#pools : [
		'ZipFileConstants'
	],
	#category : #'Compression-Archives'
}

{ #category : #'instance creation' }
ZipArchiveMember class >> newFromDirectory: aFileReference [
	^ZipDirectoryMember newFromDirectory: aFileReference
]

{ #category : #'instance creation' }
ZipArchiveMember class >> newFromFile: aFileReference [
	^ZipNewFileMember newFromFile: aFileReference
]

{ #category : #'instance creation' }
ZipArchiveMember class >> newFromString: aString named: aFileName [
	^ZipStringMember newFrom: aString named: aFileName
]

{ #category : #'instance creation' }
ZipArchiveMember class >> newFromZipFile: stream named: fileName [
	^ZipFileMember newFrom: stream named: fileName
]

{ #category : #private }
ZipArchiveMember >> asDirectory [
	^ZipDirectoryMember new copyFrom: self
]

{ #category : #'extraction-ui' }
ZipArchiveMember >> askOverwriteOptionsUsing: aUIManager onYes: yesBlock onNo: noBlock onRetryWithOverwrite: retryWithOverwriteBlock onCancel: cancelBlock [
	| index |
	[ index := aUIManager chooseFrom: {'Yes, overwrite' . 'No, don''t overwrite' . 'Overwrite ALL files' . 'Cancel operation'} lines: #(2) title: fileName , ' already exists. Overwrite?'.
	index isNil ] whileTrue.
	index = 1
		ifTrue: [ yesBlock value ].
	index = 2
		ifTrue: [ noBlock value ].
	index = 3
		ifTrue: [ retryWithOverwriteBlock value ].
	index = 4
		ifTrue: [ cancelBlock value ]
]

{ #category : #'accessing - spec fields' }
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
	
	^ bitFlag.
]

{ #category : #'accessing - spec fields' }
ZipArchiveMember >> bitFlag: aNumber [
	"See comment for #bitFlag"
	
	bitFlag := aNumber
]

{ #category : #accessing }
ZipArchiveMember >> centralDirectoryHeaderSize [

	| systemFileName systemFileComment systemCdExtraField |
	systemFileName := fileName asVmPathName.
	systemFileComment := fileComment convertToSystemString.
	systemCdExtraField := cdExtraField.
	^ 46 + systemFileName size + systemCdExtraField size + systemFileComment size

]

{ #category : #accessing }
ZipArchiveMember >> clearExtraFields [
	cdExtraField := ''.
	localExtraField := ''
]

{ #category : #'private-writing' }
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

{ #category : #accessing }
ZipArchiveMember >> compressedSize [
	"Return the compressed size for this member.
	This will not be set for members that were constructed from strings
	or external files until after the member has been written."
	^compressedSize
]

{ #category : #accessing }
ZipArchiveMember >> compressionMethod [
	"Returns my compression method. This is the method that is
	currently being used to compress my data.

	This will be CompressionStored for added string or file members,
	or CompressionStored or CompressionDeflated (others are possible but not handled)"

	^compressionMethod
]

{ #category : #accessing }
ZipArchiveMember >> contentStream [
	"Answer my contents as a text stream.
	Default is no conversion, since we don't know what the bytes mean."

	^self contentStreamFromEncoding: 'latin1'

]

{ #category : #accessing }
ZipArchiveMember >> contentStreamFromEncoding: encodingName [
	"Answer my contents as a text stream.
	Interpret the raw bytes with given encodingName"

	^ ((ByteArray new: self uncompressedSize streamContents: [ :stream |
		self extractTo: stream ]) decodeWith: encodingName)
			readStream
]

{ #category : #reading }
ZipArchiveMember >> contents [
	"Answer my contents as a string."
	^ ByteArray new: self uncompressedSize streamContents: [ :s | 
		self extractTo: s ]
]

{ #category : #reading }
ZipArchiveMember >> contentsFrom: start to: finish [
	"Answer my contents as a string."
	| s |
	s := (String new: finish - start + 1) writeStream.
	self extractTo: s from: start to: finish.
	^s contents
]

{ #category : #'private-writing' }
ZipArchiveMember >> copyDataTo: aStream [

	compressionMethod = CompressionStored ifTrue: [ ^self copyDataWithCRCTo: aStream ].

	self copyRawDataTo: aStream
]

{ #category : #'private-writing' }
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

{ #category : #'private-writing' }
ZipArchiveMember >> copyRawDataTo: aStream [

	[ readDataRemaining > 0 ] whileTrue: [ | data |
		data := self readRawChunk: (4096 min: readDataRemaining).
		aStream nextPutAll: data.
		readDataRemaining := readDataRemaining - data size ]
]

{ #category : #'private-writing' }
ZipArchiveMember >> copyRawDataTo: aStream from: start to: finish [

	readDataRemaining := readDataRemaining min: finish - start + 1.

	self readRawChunk: start - 1.

	[ readDataRemaining > 0 ] whileTrue: [ | data |
		data := self readRawChunk: (32768 min: readDataRemaining).
		aStream nextPutAll: data.
		readDataRemaining := readDataRemaining - data size ]

]

{ #category : #accessing }
ZipArchiveMember >> crc32 [
	^crc32
]

{ #category : #accessing }
ZipArchiveMember >> crc32String [
	| hexString |
	hexString := crc32 storeStringHex.
	^('00000000' copyFrom: 1 to: 11 - (hexString size)) , (hexString copyFrom: 4 to: hexString size)
]

{ #category : #accessing }
ZipArchiveMember >> desiredCompressionLevel [
	^desiredCompressionLevel
]

{ #category : #accessing }
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

{ #category : #accessing }
ZipArchiveMember >> desiredCompressionMethod [
	"Get my desiredCompressionMethod.
	This is the method that will be used to write"

	^desiredCompressionMethod
]

{ #category : #accessing }
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

{ #category : #private }
ZipArchiveMember >> endRead [
	readDataRemaining := 0
]

{ #category : #extraction }
ZipArchiveMember >> extractInDirectory: dir [
	self extractToFileNamed: self localFileName inDirectory: dir overwrite: true
]

{ #category : #'extraction-ui' }
ZipArchiveMember >> extractInDirectory: aDirectory informingOverwrite: shouldOverwrite onSuccess: successBlock onRetryWithOverwrite: retryWithOverwriteBlock onFailedOverwrite: failedBlock onAbortOverwrite: abortBlock [
	"Extract this entry into the given directory. Answer successBlock or call one of the other action blocks."

	| path fileDir file localName |
	path := fileName findTokens: '/'.
	localName := path last.
	fileDir := path allButLast inject: aDirectory into: [ :base :part | base / part ].
	fileDir ensureCreateDirectory.
	file := fileDir / localName.
	^ self
		extractToFile: file
		informingOverwrite: shouldOverwrite
		onSuccess: successBlock
		onRetryWithOverwrite: retryWithOverwriteBlock
		onFailedOverwrite: failedBlock
		onAbortOverwrite: abortBlock
]

{ #category : #'extraction-ui' }
ZipArchiveMember >> extractInDirectory: aDirectory overwrite: shouldOverwrite [
	"Extract this entry into the given directory. Answer #okay, #failed, #abort, or #retryWithOverwrite."

	^ self
		extractInDirectory: aDirectory
		informingOverwrite: shouldOverwrite
		onSuccess: [ ^ #okay ]
		onRetryWithOverwrite: [ ^ #retryWithOverwrite ]
		onFailedOverwrite: [ ^ #failed ]
		onAbortOverwrite: [ ^ #abort ]
]

{ #category : #extraction }
ZipArchiveMember >> extractInDirectory: dir withoutInformingOverwrite: overwrite [
	"Extract myself to dir without informing user whether to overwrite existing file"

	self extractToFileNamed: self localFileName inDirectory: dir overwrite: overwrite
]

{ #category : #extraction }
ZipArchiveMember >> extractTo: aStream [
	| oldCompression |
	self isEncrypted ifTrue: [ self error: 'encryption is unsupported' ].
	oldCompression := self desiredCompressionMethod: CompressionStored.
	self rewindData.
	self writeDataTo: aStream.
	self desiredCompressionMethod: oldCompression.
	self endRead
]

{ #category : #extraction }
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

{ #category : #'private-extraction' }
ZipArchiveMember >> extractToFile: aFile informingOverwrite: shouldOverwrite onSuccess: successBlock onRetryWithOverwrite: retryWithOverwriteBlock onFailedOverwrite: failedBlock onAbortOverwrite: abortBlock [
	"Extract this entry into the given file. Answer successBlock or call one of the other action blocks."

	| file |
	file := aFile.
	(file exists and: [ shouldOverwrite not ])
		ifTrue: [ | overwriteBlock |
			overwriteBlock := [ file := [ file
				ensureDelete;
				ensureCreateFile ]
				on: Error
				do: [ :ex | 
					failedBlock cull: ex.
					^ self ] ].
			self
				askOverwriteOptionsUsing: UIManager default
				onYes: overwriteBlock
				onNo: [ ^ #okay ]
				onRetryWithOverwrite: [ retryWithOverwriteBlock value.
					overwriteBlock value ]
				onCancel: abortBlock ].
	file binaryWriteStreamDo: [ :str | self extractTo: str ].
	^ successBlock value
]

{ #category : #extraction }
ZipArchiveMember >> extractToFileNamed: aFileName [
	self extractToFileNamed: aFileName inDirectory: FileSystem workingDirectory overwrite: true
]

{ #category : #extraction }
ZipArchiveMember >> extractToFileNamed: aLocalFileName inDirectory: dir [
	self extractToFileNamed: aLocalFileName inDirectory: dir overwrite: true
]

{ #category : #extraction }
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

{ #category : #accessing }
ZipArchiveMember >> fileComment [
	^fileComment
]

{ #category : #accessing }
ZipArchiveMember >> fileComment: aString [
	fileComment := aString
]

{ #category : #testing }
ZipArchiveMember >> hasDataDescriptor [
	^ (self bitFlag bitAnd: 8)	~= 0 "GPBF:=HAS:=DATA:=DESCRIPTOR:=MASK"
]

{ #category : #initialization }
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

{ #category : #testing }
ZipArchiveMember >> isDirectory [
	^false
]

{ #category : #testing }
ZipArchiveMember >> isEncrypted [
	"Return true if this member is encrypted (this is unsupported)"
	^ (self bitFlag bitAnd: 1) ~= 0
]

{ #category : #testing }
ZipArchiveMember >> isTextFile [
	"Returns true if I am a text file.
	Note that this module does not currently do anything with this flag
	upon extraction or storage.
	That is, bytes are stored in native format whether or not they came
	from a text file."
	^ (internalFileAttributes bitAnd: 1) ~= 0

]

{ #category : #testing }
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

{ #category : #accessing }
ZipArchiveMember >> lastModTime [
	"Return my last modification date"

	^ lastModFileDateTime
]

{ #category : #accessing }
ZipArchiveMember >> localFileName [
	"Answer my fileName in terms of the local directory naming convention"
	^ fileName copyReplaceAll: '/' with: FileSystem disk delimiter asString
]

{ #category : #testing }
ZipArchiveMember >> looksLikeDirectory [
	^false
]

{ #category : #private }
ZipArchiveMember >> mapPermissionsFromUnix: unixPerms [
	"Take Unix permissions (e.g. 8r40755 for a directory) and put them in the high 16 bits. The low 16 bits are for DOS permissions"
	
	^ unixPerms bitShift: 16
]

{ #category : #private }
ZipArchiveMember >> mapPermissionsToUnix: zipPerms [
	"Extract Unix permissions (the high 16 bits) from zip permissions. The low 16 bits are for DOS permissions"
	
	^ zipPerms bitShift: -16
]

{ #category : #accessing }
ZipArchiveMember >> modifiedAt: aDateAndTime [

	lastModFileDateTime := aDateAndTime
]

{ #category : #private }
ZipArchiveMember >> readRawChunk: n [
	self subclassResponsibility
]

{ #category : #private }
ZipArchiveMember >> rewindData [
	readDataRemaining :=  (desiredCompressionMethod = CompressionDeflated
		and: [ compressionMethod = CompressionDeflated ])
			ifTrue: [ compressedSize ]
			ifFalse: [ uncompressedSize ]

]

{ #category : #accessing }
ZipArchiveMember >> splitFileName [
	"Answer my name split on slash boundaries. A directory will have a trailing empty string."
	^ fileName findTokens: '/'
]

{ #category : #accessing }
ZipArchiveMember >> uncompressedSize [
	"Return the uncompressed size for this member."
	^uncompressedSize
]

{ #category : #accessing }
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

{ #category : #accessing }
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

{ #category : #'accessing - spec fields' }
ZipArchiveMember >> versionMadeBy [
	"Per .ZIP File Format Specification Version: 6.3.2:
		2 Bytes:
		- The upper byte indicates the compatibility of the file attribute information.  If the external file attributes are compatible with MS-DOS and can be read by PKZIP for DOS version 2.04g then this value will be zero.
		- The lower byte indicates the ZIP specification version (the version of PKWARE Appnote) supported by the software used to encode the file.  The value / 10 indicates the major version number, and the value mod 10 is the minor version number"
	
	^ versionMadeBy
]

{ #category : #'accessing - spec fields' }
ZipArchiveMember >> versionMadeBy: anInteger [
	"See comment for #versionMadeBy"
	
	versionMadeBy := anInteger
]

{ #category : #'accessing - spec fields' }
ZipArchiveMember >> versionNeededToExtract [
	"See .ZIP File Format Specification Version: 6.3.2.
		For example, 2.0 means:
		- File is a folder (directory)
		- File is compressed using Deflate compression
		- File is encrypted using traditional PKWARE encryption"
	
	^ versionNeededToExtract
]

{ #category : #'accessing - spec fields' }
ZipArchiveMember >> versionNeededToExtract: anInteger [
	"See comment for #versionNeededToExtract"
	
	versionNeededToExtract := anInteger
]

{ #category : #'private-writing' }
ZipArchiveMember >> writeCentralDirectoryFileHeaderTo: aStream [
	"C2 v3 V4 v5 V2"

	| systemFileName systemFileComment systemCdExtraField endianStream |
	systemFileName := fileName asVmPathName.
	systemFileComment := fileComment convertToSystemString.
	systemCdExtraField := cdExtraField.
	aStream nextPutAll: CentralDirectoryFileHeaderSignature.
	

	endianStream := ZnEndianessReadWriteStream on: aStream.
	endianStream nextLittleEndianNumber: 1 put: self versionMadeBy.
	endianStream nextLittleEndianNumber: 1 put: fileAttributeFormat.

	endianStream nextLittleEndianNumber: 2 put: versionNeededToExtract.
	endianStream nextLittleEndianNumber: 2 put: self bitFlag.
	endianStream nextLittleEndianNumber: 2 put: desiredCompressionMethod.

	endianStream nextLittleEndianNumber: 4 put: lastModFileDateTime asDosTimestamp.

	"These next 3 should have been updated during the write of the data"
	endianStream nextLittleEndianNumber: 4 put: crc32.
	endianStream nextLittleEndianNumber: 4 put: (desiredCompressionMethod = CompressionStored
												ifTrue: [ uncompressedSize ] ifFalse: [ compressedSize ]).
	endianStream nextLittleEndianNumber: 4 put: uncompressedSize.

	endianStream nextLittleEndianNumber: 2 put: systemFileName size.
	endianStream nextLittleEndianNumber: 2 put: systemCdExtraField size.
	endianStream nextLittleEndianNumber: 2 put: systemFileComment size.
	endianStream nextLittleEndianNumber: 2 put: 0.		"diskNumberStart"
	endianStream nextLittleEndianNumber: 2 put: internalFileAttributes.

	endianStream nextLittleEndianNumber: 4 put: externalFileAttributes.
	endianStream nextLittleEndianNumber: 4 put: headerStartPosition.

	aStream nextPutAll: systemFileName asByteArray.
	aStream nextPutAll: systemCdExtraField asByteArray.
	aStream nextPutAll: systemFileComment asByteArray
]

{ #category : #'private-writing' }
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

{ #category : #'private-writing' }
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

	self copyDataTo: aStream.
]

{ #category : #'private-writing' }
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

	self copyRawDataTo: aStream from: start to: finish.
]

{ #category : #'private-writing' }
ZipArchiveMember >> writeLocalFileHeaderTo: aStream [
	"Write my local header to a file handle.
	Stores the offset to the start of the header in my
	writeLocalHeaderRelativeOffset member."

	| systemFileName endianStream |
	systemFileName := fileName asVmPathName.
	aStream nextPutAll: LocalFileHeaderSignature.

	endianStream := ZnEndianessReadWriteStream on: aStream.
	endianStream nextLittleEndianNumber: 2 put: versionNeededToExtract.
	endianStream nextLittleEndianNumber: 2 put: self bitFlag.
	endianStream nextLittleEndianNumber: 2 put: desiredCompressionMethod.

	endianStream nextLittleEndianNumber: 4 put: lastModFileDateTime asDosTimestamp.
	endianStream nextLittleEndianNumber: 4 put: crc32.
	endianStream nextLittleEndianNumber: 4 put: (desiredCompressionMethod = CompressionStored
												ifTrue: [ uncompressedSize ] ifFalse: [ compressedSize ]).
	endianStream nextLittleEndianNumber: 4 put: uncompressedSize.

	endianStream nextLittleEndianNumber: 2 put: systemFileName size.
	endianStream nextLittleEndianNumber: 2 put: localExtraField size.

	aStream nextPutAll: systemFileName asByteArray.
	aStream nextPutAll: localExtraField asByteArray.

]

{ #category : #writing }
ZipArchiveMember >> writeTo: aStream [
	"The header contains information about the data, including compressed size and CRC,
	 that we do not know yet! So we leave room for the header, write the data, and go
	 back to write the header after we have the needed information."

	| headerSize headerEndPosition dataEndPosition |
	self rewindData.
	headerStartPosition := aStream position.
	headerSize := 30
		+ fileName asVmPathName asByteArray size 
		+ localExtraField asByteArray size.
	headerEndPosition := headerStartPosition + headerSize.
	aStream nextPutAll: (ByteArray new: headerSize).
	self writeDataTo: aStream.
	dataEndPosition := aStream position.
	aStream position: headerStartPosition.
	self writeLocalFileHeaderTo: aStream.
	"Verify that our calculation of the header size was correct!"
	aStream position == headerEndPosition ifFalse: [ self error: 'Incorrect header size!' ].
	aStream position: dataEndPosition.
]
