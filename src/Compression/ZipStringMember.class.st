"
ZipStringMember instances are used to represent files that are going to be written to a ZipArchive.
Their data comes from in-image strings, though.
"
Class {
	#name : 'ZipStringMember',
	#superclass : 'ZipArchiveMember',
	#instVars : [
		'contents',
		'stream'
	],
	#category : 'Compression-Archives',
	#package : 'Compression',
	#tag : 'Archives'
}

{ #category : 'instance creation' }
ZipStringMember class >> newFrom: aString named: aFileName [
	^(self new) contents: aString; localFileName: aFileName; yourself
]

{ #category : 'initialization' }
ZipStringMember >> contents [
	^contents
]

{ #category : 'initialization' }
ZipStringMember >> contents: aStringOrByteArray [
	"Set my contents: a String is text, written in UTF-8; a ByteArray is written as is"

	contents := aStringOrByteArray.
	compressedSize := uncompressedSize := self data size.
	"set the file date to now"
	self modifiedAt: DateAndTime now
]

{ #category : 'private - writing' }
ZipStringMember >> data [
	"Answer my contents as they are written"

	^ contents isString
		  ifTrue: [ contents utf8Encoded ]
		  ifFalse: [ contents asByteArray ]
]

{ #category : 'initialization' }
ZipStringMember >> initialize [
	super initialize.
	self contents: ''.
	compressionMethod := desiredCompressionMethod := CompressionStored
]

{ #category : 'private' }
ZipStringMember >> readRawChunk: n [
	^stream next: n
]

{ #category : 'private - writing' }
ZipStringMember >> rewindData [

	| data |
	super rewindData.
	data := self data.
	stream := data readStream.
	readDataRemaining := data size
]
