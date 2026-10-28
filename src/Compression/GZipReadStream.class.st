"
gzip is an integral part of the VM. 
"
Class {
	#name : 'GZipReadStream',
	#superclass : 'FastInflateStream',
	#instVars : [
		'header'
	],
	#pools : [
		'GZipConstants'
	],
	#category : 'Compression-Streams',
	#package : 'Compression',
	#tag : 'Streams'
}

{ #category : 'file in/out' }
GZipReadStream class >> fileIn: fullFileName [
	"FileIn the contents of a gzipped file"

	self
		with: fullFileName asFileReference binaryReadStream
		do: [ :aStream | CodeImporter evaluateString: aStream contents asString ]
]

{ #category : 'unzipping' }
GZipReadStream class >> unzip: fullFileName [
	"Unzip the contents of the file specified by the full path name or file reference fullFileName to the current working directory
	To unzip a .zip go to the class ZipArchive"

	^ self unzip: fullFileName to: FileSystem disk workingDirectory
]

{ #category : 'unzipping' }
GZipReadStream class >> unzip: fullFileName to: pathName [
	"Unzip the contents of the file specified by the full path name or file reference fullFileName to the location given by pathName.
	GZipReadStream unzip:'/aPath/zip.gz' to: '/aPath'
	GZipReadStream unzip:'zip.gz' to: '/aPath'
	GZipReadStream unzip:'/aPath/zip.gz' asFileReference to: '/aPath' asFileReference

	To unzip a .zip go to the class ZipArchive"

	| buffer newName |
	newName := fullFileName asFileReference basenameWithoutExtension.
	pathName asFileReference ensureCreateDirectory / newName
		binaryWriteStreamDo: [ :unzipped |
			self with: fullFileName asFileReference binaryReadStream
				do: [ :zipped |
					buffer := ByteArray new: 50000.
					[ zipped atEnd ] whileFalse: [ unzipped nextPutAll: (zipped nextInto: buffer) ] ] ].
	^ newName
]

{ #category : 'accessing' }
GZipReadStream >> header [
	"Answer the GZipHeader of the first member: original file name, comment, modification time..."

	^ header
]

{ #category : 'private' }
GZipReadStream >> nextBytes: anInteger [
	"Read anInteger header bytes"

	^ (1 to: anInteger) collect: [ :each | self nextBits: 8 ] as: ByteArray
]

{ #category : 'private' }
GZipReadStream >> nextZeroTerminatedString [
	"Read a zero terminated ISO 8859-1 header string (FNAME and FCOMMENT)"

	^ String streamContents: [ :stream |
			  | byte |
			  [ (byte := self nextBits: 8) = 0 ] whileFalse: [ stream nextPut: (Character value: byte) ] ]
]

{ #category : 'initialization' }
GZipReadStream >> on: aCollection from: firstIndex to: lastIndex [
	"Check the header of the GZIP stream."

	super on: aCollection from: firstIndex to: lastIndex.
	header := self readHeader
]

{ #category : 'initialization' }
GZipReadStream >> readHeader [
	"Read the header of a GZIP member (RFC 1952, section 2.3.1) and answer it"

	| headerStart method magic flags modificationTime newHeader |
	crc := 16rFFFFFFFF.
	headerStart := sourcePos + 1.
	magic := self nextBits: 16.
	magic = GZipMagic ifFalse: [ ^ self error: 'Not a GZipped stream' ].
	method := self nextBits: 8.
	method = GZipDeflated ifFalse: [ ^ self error: 'Bad compression method' ].
	flags := self nextBits: 8.
	(flags anyMask: GZipReservedFlags) ifTrue: [ ^ self error: 'Cannot decompress stream with reserved flags set' ].
	newHeader := GZipHeader new.
	newHeader isText: (flags anyMask: GZipAsciiFlag).
	modificationTime := self nextBits: 32. "MTIME"
	modificationTime = 0 ifFalse: [ newHeader modificationTime: (DateAndTime fromUnixTime: modificationTime) ].
	self nextBits: 8. "XFL"
	newHeader operatingSystem: (self nextBits: 8).
	(flags anyMask: GZipExtraField) ifTrue: [ newHeader extraField: (self nextBytes: (self nextBits: 16)) ].
	(flags anyMask: GZipNameFlag) ifTrue: [ newHeader fileName: self nextZeroTerminatedString ].
	(flags anyMask: GZipCommentFlag) ifTrue: [ newHeader comment: self nextZeroTerminatedString ].
	(flags anyMask: GZipHeaderCrcFlag) ifTrue: [ self verifyHeaderCrcFrom: headerStart ].
	^ newHeader
]

{ #category : 'private' }
GZipReadStream >> skipSizeAndStartNextMember [
	"Skip the ISIZE field ending the current member.
	A GZIP file is a series of members (RFC 1952, section 2.2): if another member follows, read its header and continue decompressing."

	sourceLimit - sourcePos >= 4 ifFalse: [ ^ self ].
	sourcePos := sourcePos + 4.
	self startsWithMagic ifFalse: [ ^ self ].
	bitBuf := bitPos := 0.
	self readHeader.
	state := StateNewBlock
]

{ #category : 'private' }
GZipReadStream >> startsWithMagic [
	"Answer whether the next source bytes are the GZIP magic number, i.e. whether another member follows"

	^ sourceLimit - sourcePos >= 2 and: [
		  (source byteAt: sourcePos + 1) = (GZipMagic bitAnd: 16rFF) and: [ (source byteAt: sourcePos + 2) = (GZipMagic bitShift: -8) ] ]
]

{ #category : 'crc' }
GZipReadStream >> updateCrc: oldCrc from: start to: stop in: aCollection [
	"Answer an updated CRC for the range of bytes in aCollection"
	^ZipWriteStream updateCrc: oldCrc from: start to: stop in: aCollection
]

{ #category : 'crc' }
GZipReadStream >> verifyCrc [

	| stored |
	stored := 0.
	0 to: 24 by: 8 do: [ :i |
			sourcePos >= sourceLimit ifTrue: [ ^ self crcError: 'No checksum (proceed to ignore)' ].
			stored := stored + (self nextByte bitShift: i) ].
	stored := stored bitXor: 16rFFFFFFFF.
	stored = crc ifFalse: [ ^ self crcError: 'Wrong checksum (proceed to ignore)' ].
	self skipSizeAndStartNextMember.
	^ stored
]

{ #category : 'crc' }
GZipReadStream >> verifyHeaderCrcFrom: headerStart [
	"FHCRC: the header ends with the two least significant bytes of the CRC-32 of all the header bytes before them"

	| expected actual |
	actual := (ZipWriteStream
		           updateCrc: 16rFFFFFFFF
		           from: headerStart
		           to: sourcePos
		           in: source) bitXor: 16rFFFFFFFF.
	expected := self nextBits: 16.
	(actual bitAnd: 16rFFFF) = expected ifFalse: [ self crcError: 'Wrong header CRC (proceed to ignore)' ]
]
