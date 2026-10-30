"
Tests for the GZipReadStream class
"
Class {
	#name : 'GZipReadStreamTest',
	#superclass : 'TestCase',
	#instVars : [
		'filesystem'
	],
	#category : 'Compression-Tests-Streams',
	#package : 'Compression-Tests',
	#tag : 'Streams'
}

{ #category : 'resources' }
GZipReadStreamTest class >> gzFileBytes [
	^ #[31 139 8 8 9 73 195 91 4 0 116 101 115 116 46 116 120 116 0 1 19 0 236 255 84 104 105 115 32 105 115 32 97 32 116 101 115 116 32 116 101 120 116 30 128 114 177 19 0 0 0]
]

{ #category : 'resources' }
GZipReadStreamTest class >> gzFileName [
	^ 'test.txt.gz'
]

{ #category : 'running' }
GZipReadStreamTest >> setUp [
	super setUp.
	filesystem := FileSystem memory
]

{ #category : 'tests' }
GZipReadStreamTest >> testGzipDataStartsWithGzipMagic [
	"GZipConstants gzipMagic is 16r8B1F: the bytes 16r1F 16r8B read as a little-endian number (RFC 1952)"

	self assert: GZipConstants gzipMagic equals: 16r8B1F.
	self assert: (self class gzFileBytes first: 2) asByteArray equals: #[ 16r1F 16r8B ]
]

{ #category : 'tests' }
GZipReadStreamTest >> testUnzipTo [
	| gzFile txtFile return |
	[ (gzFile := filesystem workingDirectory / self class gzFileName)
		binaryWriteStreamDo: [ :stream | stream nextPutAll: self class gzFileBytes ].
	txtFile := filesystem workingDirectory / gzFile basenameWithoutExtension.

	return := GZipReadStream unzip: gzFile to: filesystem workingDirectory.
	self assert: return equals: gzFile basenameWithoutExtension.
	self assert: txtFile exists.
	self assert: txtFile contents equals: 'This is a test text' ]
		ensure: [ filesystem workingDirectory ensureDeleteAllChildren ]
]
