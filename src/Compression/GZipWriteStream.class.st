"
gzip is an integral part of the VM. 
"
Class {
	#name : 'GZipWriteStream',
	#superclass : 'ZipWriteStream',
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

{ #category : 'instance creation' }
GZipWriteStream class >> on: aCollectionOrStream header: aGZipHeader [
	"Answer a stream writing GZIP data with the file name, comment, modification time... of aGZipHeader"

	^ self basicNew
		  setHeader: aGZipHeader;
		  on: aCollectionOrStream
]

{ #category : 'accessing' }
GZipWriteStream >> header [
	"Answer the GZipHeader written at the beginning of the data"

	^ header ifNil: [ header := GZipHeader new ]
]

{ #category : 'initialization' }
GZipWriteStream >> setHeader: aGZipHeader [

	header := aGZipHeader
]

{ #category : 'initialization' }
GZipWriteStream >> writeFooter [
	"Write some footer information for the crc"
	super writeFooter.
	0 to: 3 do:[:i| encoder nextBytePut: (crc >> (i*8) bitAnd: 255)].
	0 to: 3 do:[:i| encoder nextBytePut: (bytesWritten >> (i*8) bitAnd: 255)]
]

{ #category : 'initialization' }
GZipWriteStream >> writeHeader [
	"Write the GZip header (RFC 1952, section 2.3.1)"

	encoder nextBits: 16 put: GZipMagic.
	encoder nextBits: 8 put: GZipDeflated.
	encoder nextBits: 8 put: self header flags.
	encoder nextBits: 32 put: self header unixModificationTime.
	encoder nextBits: 8 put: 0. "No extra flags"
	encoder nextBits: 8 put: self header operatingSystem.
	self header extraField ifNotNil: [ :bytes |
			bytes size > 16rFFFF ifTrue: [ ^ self error: 'GZIP extra field is limited to 65535 bytes' ].
			encoder nextBits: 16 put: bytes size.
			bytes do: [ :each | encoder nextBits: 8 put: each ] ].
	self header fileName ifNotNil: [ :aString | self writeZeroTerminatedString: aString ].
	self header comment ifNotNil: [ :aString | self writeZeroTerminatedString: aString ]
]

{ #category : 'initialization' }
GZipWriteStream >> writeZeroTerminatedString: aString [
	"Write an ISO 8859-1 header string (FNAME and FCOMMENT)"

	aString do: [ :each |
			(each asInteger between: 1 and: 255) ifFalse: [
				^ self error: 'GZIP file names and comments must be ISO 8859-1 strings without zero character' ].
			encoder nextBits: 8 put: each asInteger ].
	encoder nextBits: 8 put: 0
]
