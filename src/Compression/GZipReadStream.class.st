"
gzip is an integral part of the VM. 
"
Class {
	#name : #GZipReadStream,
	#superclass : #FastInflateStream,
	#pools : [
		'GZipConstants'
	],
	#category : #'Compression-Streams'
}

{ #category : #'file in/out' }
GZipReadStream class >> fileIn: fullFileName [
	"FileIn the contents of a gzipped file"

	self
		with: fullFileName asFileReference binaryReadStream
		do: [ :aStream | CodeImporter evaluateString: aStream contents asString ]
]

{ #category : #'file in/out' }
GZipReadStream class >> fileIntoNewChangeSet: fullFileName [
	"FileIn the contents of a gzipped file"

	| csClass |
	csClass := Smalltalk globals at: #ChangeSet ifAbsent: [ ^ self ].
	fullFileName asFileReference
		binaryReadStreamDo: [ :aBinaryStream | 
			self with: aBinaryStream
				do: [ :aGzStream | 
					aGzStream contents asString
						readStreamDo: [ :aReadStream | 
							aReadStream reset.
							csClass newChangesFromStream: aReadStream named: fullFileName asFileReference basename ] ] ]
]

{ #category : #'file in/out' }
GZipReadStream class >> saveContents: fullFileName [
	"Save the contents of a gzipped file"

	| buffer newName |
	newName := fullFileName asFileReference basenameWithoutExtension.
	newName asFileReference binaryWriteStreamDo: [ :unzipped | 
			fullFileName asFileReference
				binaryReadStreamDo: [ :readStream | 
					self with: readStream
						do: [ :zipped | 
							buffer := ByteArray new: 50000.
							'Extracting ' , fullFileName
								displayProgressFrom: 0
								to: zipped sourceStream size
								during: [ :bar | 
									[ zipped atEnd ]
										whileFalse: [ bar current: zipped sourceStream position.
											unzipped nextPutAll: (zipped nextInto: buffer) ] ] ] ] ].
	^ newName
]

{ #category : #'file in/out' }
GZipReadStream class >> uncompressedFileName: fullName [
	^((fullName endsWith: '.gz') and: [self confirm: ('{1}
appears to be a compressed file.
Do you want to uncompress it?' format:{fullName})])
		ifFalse: [fullName]
		ifTrue:[self saveContents: fullName]
]

{ #category : #unzipping }
GZipReadStream class >> unzip: fullFileName [ 
	"Unzip the contents of the file specified by the full path name or file reference fullFileName to the current working directory
	To unzip a .zip go to the class ZipArchive"
	
	^ self unzip: fullFileName to: FileSystem disk workingDirectory 
]

{ #category : #unzipping }
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

{ #category : #'file in/out' }
GZipReadStream class >> viewContents: fullFileName [
	"Open the decompressed contents of the .gz file with the given name.  This method is only required for the registering-file-list of Squeak 3.3a and beyond, but does no harm in an earlier system"

	| file |
	(file := fullFileName asFileReference) binaryReadStreamDo: 
		[ :aStream | self with: aStream do: 
			[ :aGzStream | UIManager default edit: aGzStream upToEnd label: 'Decompressed contents of: ' , file basename ] ]
]

{ #category : #initialization }
GZipReadStream >> on: aCollection from: firstIndex to: lastIndex [
	"Check the header of the GZIP stream."
	| method magic flags length |
	super on: aCollection from: firstIndex to: lastIndex.
	crc := 16rFFFFFFFF.
	magic := self nextBits: 16.
	(magic = GZipMagic) 
		ifFalse:[^self error:'Not a GZipped stream'].
	method := self nextBits: 8.
	(method = GZipDeflated)
		ifFalse:[^self error:'Bad compression method'].
	flags := self nextBits: 8.
	(flags anyMask: GZipEncryptFlag) 
		ifTrue:[^self error:'Cannot decompress encrypted stream'].
	(flags anyMask: GZipReservedFlags)
		ifTrue:[^self error:'Cannot decompress stream with unknown flags'].
	"Ignore stamp, extra flags, OS type"
	self nextBits: 16; nextBits: 16. "stamp"
	self nextBits: 8. "extra flags"
	self nextBits: 8. "OS type"
	(flags anyMask: GZipContinueFlag) "Number of multi-part archive - ignored"
		ifTrue:[self nextBits: 16]. 
	(flags anyMask: GZipExtraField) "Extra fields - ignored"
		ifTrue:[	length := self nextBits: 16.
				1 to: length do:[:i| self nextBits: 8]].
	(flags anyMask: GZipNameFlag) "Original file name - ignored"
		ifTrue:[[(self nextBits: 8) = 0] whileFalse].
	(flags anyMask: GZipCommentFlag) "Comment - ignored"
		ifTrue:[[(self nextBits: 8) = 0] whileFalse]

]

{ #category : #crc }
GZipReadStream >> updateCrc: oldCrc from: start to: stop in: aCollection [
	"Answer an updated CRC for the range of bytes in aCollection"
	^ZipWriteStream updateCrc: oldCrc from: start to: stop in: aCollection.
]

{ #category : #crc }
GZipReadStream >> verifyCrc [
	| stored |
	stored := 0.
	0 to: 24 by: 8 do: [ :i |
		sourcePos >= sourceLimit ifTrue: [ ^ self crcError: 'No checksum (proceed to ignore)' ].
		stored := stored + (self nextByte bitShift: i) ].
	stored := stored bitXor: 16rFFFFFFFF.
	stored = crc ifFalse: [ ^ self crcError: 'Wrong checksum (proceed to ignore)' ].
	^stored
]
