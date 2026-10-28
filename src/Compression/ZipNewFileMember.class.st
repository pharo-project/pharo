"
ZipNewFileMember instances are used to represent files that are going to be written to a ZipArchive.
Their data comes from external file streams.
"
Class {
	#name : 'ZipNewFileMember',
	#superclass : 'ZipArchiveMember',
	#instVars : [
		'externalFileName',
		'stream',
		'reference'
	],
	#category : 'Compression-Archives',
	#package : 'Compression',
	#tag : 'Archives'
}

{ #category : 'instance creation' }
ZipNewFileMember class >> newFromFile: aFileReference [

	^ self new from: aFileReference
]

{ #category : 'initialization' }
ZipNewFileMember >> close [
	stream ifNotNil:[stream close]
]

{ #category : 'initialization' }
ZipNewFileMember >> endRead [

	super endRead.
	self close
]

{ #category : 'initialization' }
ZipNewFileMember >> from: aFileReference [

	| entry |
	"Now get the size, attributes, and timestamps, and see if the file exists"
	externalFileName := aFileReference.
	self localFileName: aFileReference fullName.
	reference := aFileReference.
	entry := aFileReference entry.
	compressedSize := uncompressedSize := entry size.
	desiredCompressionMethod := compressedSize > 0
		                            ifTrue: [ CompressionDeflated ]
		                            ifFalse: [ CompressionStored ].
	self modifiedAt: entry modificationTime
]

{ #category : 'initialization' }
ZipNewFileMember >> initialize [

	super initialize.
	externalFileName := ''.
	compressionMethod := CompressionStored
]

{ #category : 'private' }
ZipNewFileMember >> readRawChunk: n [
	stream ifNil: [ stream := reference binaryReadStream ].
	^stream next: n
]

{ #category : 'testing' }
ZipNewFileMember >> usesFile: aFileReferenceOrFileName [
	"Do I require aFileName? That is, do I care if it's clobbered?"
	^externalFileName asFileReference = aFileReferenceOrFileName asFileReference
]
