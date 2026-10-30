"
I represent the optional information of a GZIP member header (RFC 1952, section 2.3.1).

- fileName: the original file name (FNAME), or nil
- comment: a comment (FCOMMENT), or nil
- modificationTime: the modification time of the original file (MTIME), or nil when unknown
- extraField: the raw bytes of the extra field (FEXTRA), or nil
- operatingSystem: the OS on which the compression took place (OS), 255 when unknown
- isText: whether the data is probably text (FTEXT)

File names and comments are ISO 8859-1 (Latin-1) strings, without zero byte.

GZipReadStream answers the header of the first member with #header, and GZipWriteStream writes one with #on:header:.

	| bytes reader |
	bytes := ByteArray streamContents: [ :stream |
		(GZipWriteStream on: stream header: (GZipHeader new fileName: 'notes.txt'; yourself))
			nextPutAll: 'Hello' asByteArray;
			close ].
	reader := GZipReadStream on: bytes.
	reader header fileName. ""'notes.txt'""
	reader upToEnd asString. ""'Hello'""
"
Class {
	#name : 'GZipHeader',
	#superclass : 'Object',
	#instVars : [
		'fileName',
		'comment',
		'modificationTime',
		'extraField',
		'operatingSystem',
		'isText'
	],
	#pools : [
		'GZipConstants'
	],
	#category : 'Compression-Streams',
	#package : 'Compression',
	#tag : 'Streams'
}

{ #category : 'accessing' }
GZipHeader >> comment [

	^ comment
]

{ #category : 'accessing' }
GZipHeader >> comment: aString [

	comment := aString
]

{ #category : 'accessing' }
GZipHeader >> extraField [

	^ extraField
]

{ #category : 'accessing' }
GZipHeader >> extraField: aByteArray [

	extraField := aByteArray
]

{ #category : 'accessing' }
GZipHeader >> fileName [

	^ fileName
]

{ #category : 'accessing' }
GZipHeader >> fileName: aString [

	fileName := aString
]

{ #category : 'encoding' }
GZipHeader >> flags [
	"Answer the FLG byte describing which optional fields are present"

	| flags |
	flags := 0.
	isText ifTrue: [ flags := flags bitOr: GZipAsciiFlag ].
	extraField ifNotNil: [ flags := flags bitOr: GZipExtraField ].
	fileName ifNotNil: [ flags := flags bitOr: GZipNameFlag ].
	comment ifNotNil: [ flags := flags bitOr: GZipCommentFlag ].
	^ flags
]

{ #category : 'initialization' }
GZipHeader >> initialize [

	super initialize.
	operatingSystem := 255. "unknown"
	isText := false
]

{ #category : 'accessing' }
GZipHeader >> isText [

	^ isText
]

{ #category : 'accessing' }
GZipHeader >> isText: aBoolean [

	isText := aBoolean
]

{ #category : 'accessing' }
GZipHeader >> modificationTime [

	^ modificationTime
]

{ #category : 'accessing' }
GZipHeader >> modificationTime: aDateAndTime [

	modificationTime := aDateAndTime
]

{ #category : 'accessing' }
GZipHeader >> operatingSystem [

	^ operatingSystem
]

{ #category : 'accessing' }
GZipHeader >> operatingSystem: anInteger [

	operatingSystem := anInteger
]

{ #category : 'encoding' }
GZipHeader >> unixModificationTime [
	"Answer the MTIME field: seconds since 1970-01-01 UTC, 0 when unknown"

	^ modificationTime ifNil: [ 0 ] ifNotNil: [ :time | time asUnixTime ]
]
