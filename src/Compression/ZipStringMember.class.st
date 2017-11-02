"
ZipStringMember instances are used to represent files that are going to be written to a ZipArchive.
Their data comes from in-image strings, though.
"
Class {
	#name : #ZipStringMember,
	#superclass : #ZipArchiveMember,
	#instVars : [
		'contents',
		'stream'
	],
	#category : 'Compression-Archives'
}

{ #category : #'instance creation' }
ZipStringMember class >> newFrom: aString named: aFileName [
	^(self new) contents: aString; localFileName: aFileName; yourself
]

{ #category : #initialization }
ZipStringMember >> contents [
	^contents
]

{ #category : #initialization }
ZipStringMember >> contents: aString [
	contents := aString.
	compressedSize := uncompressedSize := aString size.
	"set the file date to now"
	self modifiedAt: DateAndTime now.
]

{ #category : #initialization }
ZipStringMember >> initialize [
	super initialize.
	self contents: ''.
	compressionMethod := desiredCompressionMethod := CompressionStored.

]

{ #category : #private }
ZipStringMember >> readRawChunk: n [
	^stream next: n
]

{ #category : #private-writing }
ZipStringMember >> rewindData [
	super rewindData.
	stream := contents readStream.
	readDataRemaining := contents size
]
