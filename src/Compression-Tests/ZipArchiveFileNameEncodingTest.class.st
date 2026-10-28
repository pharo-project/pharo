"
I check the encoding of ZIP file names and comments (APPNOTE, appendix D): UTF-8 with the language encoding flag (general purpose bit 11), CP437 without it. Since many tools write UTF-8 without the flag, valid UTF-8 is read as UTF-8 too.
"
Class {
	#name : 'ZipArchiveFileNameEncodingTest',
	#superclass : 'TestCase',
	#instVars : [
		'fs',
		'zipFile'
	],
	#category : 'Compression-Tests-Archives',
	#package : 'Compression-Tests',
	#tag : 'Archives'
}

{ #category : 'helpers' }
ZipArchiveFileNameEncodingTest >> bytesOfArchiveWithMemberNamed: aName [

	| archive |
	archive := ZipArchive new.
	archive addDeflateString: 'contents' as: aName.
	^ ByteArray streamContents: [ :stream | archive writeTo: stream ]
]

{ #category : 'helpers' }
ZipArchiveFileNameEncodingTest >> clearLanguageEncodingFlagIn: aByteArray [

	| bytes |
	bytes := aByteArray copy.
	(self flagPositionsIn: bytes) do: [ :index | bytes at: index + 1 put: ((bytes at: index + 1) bitClear: 16r08) ].
	^ bytes
]

{ #category : 'helpers' }
ZipArchiveFileNameEncodingTest >> flagPositionsIn: aByteArray [
	"Answer the positions of the general purpose bit flags: 7 bytes after a local file header signature, 9 bytes after a central directory one"

	| positions |
	positions := OrderedCollection new.
	1 to: aByteArray size - 3 do: [ :index |
			(aByteArray copyFrom: index to: index + 3) = #[ 16r50 16r4B 3 4 ] ifTrue: [ positions add: index + 6 ].
			(aByteArray copyFrom: index to: index + 3) = #[ 16r50 16r4B 1 2 ] ifTrue: [ positions add: index + 8 ] ].
	^ positions
]

{ #category : 'helpers' }
ZipArchiveFileNameEncodingTest >> flagsIn: aByteArray [

	^ (self flagPositionsIn: aByteArray) collect: [ :index | (aByteArray at: index) + ((aByteArray at: index + 1) bitShift: 8) ]
]

{ #category : 'helpers' }
ZipArchiveFileNameEncodingTest >> pharoZippedByPython [
	"Bytes written by Python 3:
	zipfile.ZipFile(buffer, 'w', zipfile.ZIP_STORED).writestr('日本/été.txt', b'Pharo')"

	^ #[ 80 75 3 4 20 0 0 8 0 0 145 117 92 93 144 33 178 137 5 0 0 0 5 0 0 0 16 0 0 0 230 151 165 230 156 172 47 195 169 116 195 169
	     46 116 120 116 80 104 97 114 111 80 75 1 2 20 3 20 0 0 8 0 0 145 117 92 93 144 33 178 137 5 0 0 0 5 0 0 0 16 0 0 0 0 0 0 0 0
	     0 0 0 128 1 0 0 0 0 230 151 165 230 156 172 47 195 169 116 195 169 46 116 120 116 80 75 5 6 0 0 0 0 1 0 1 0 62 0 0 0 51 0 0
	     0 0 0 ]
]

{ #category : 'helpers' }
ZipArchiveFileNameEncodingTest >> readArchiveFrom: aByteArray do: aBlock [

	| archive |
	zipFile ensureDelete.
	zipFile binaryWriteStreamDo: [ :stream | stream nextPutAll: aByteArray ].
	archive := ZipArchive new readFrom: zipFile.
	^ [ aBlock value: archive ] ensure: [ archive close ]
]

{ #category : 'running' }
ZipArchiveFileNameEncodingTest >> setUp [

	super setUp.
	fs := FileSystem memory.
	zipFile := fs root / 'archive.zip'
]

{ #category : 'tests' }
ZipArchiveFileNameEncodingTest >> testLanguageEncodingFlagIsNotSetForAsciiNames [

	| flags |
	flags := self flagsIn: (self bytesOfArchiveWithMemberNamed: 'plain.txt').
	self assert: flags size equals: 2. "local and central headers"
	self deny: (flags anySatisfy: [ :each | each anyMask: 16r800 ])
]

{ #category : 'tests' }
ZipArchiveFileNameEncodingTest >> testLanguageEncodingFlagIsSetForNonAsciiNames [

	| flags |
	flags := self flagsIn: (self bytesOfArchiveWithMemberNamed: 'café.txt').
	self assert: flags size equals: 2. "local and central headers"
	self assert: (flags allSatisfy: [ :each | each anyMask: 16r800 ])
]

{ #category : 'tests' }
ZipArchiveFileNameEncodingTest >> testNameWithoutFlagThatIsNotUtf8IsReadAsCp437 [

	| bytes |
	"#[16r82 16r81] is not UTF-8, and is 'éü' in CP437"
	bytes := (self bytesOfArchiveWithMemberNamed: 'XY.txt') copyReplaceAll: 'XY' asByteArray with: #[ 16r82 16r81 ].
	self readArchiveFrom: bytes do: [ :archive | self assert: archive members first fileName equals: 'éü.txt' ]
]

{ #category : 'tests' }
ZipArchiveFileNameEncodingTest >> testNonAsciiCommentRoundTrip [

	| archive bytes |
	archive := ZipArchive new.
	(archive addDeflateString: 'contents' as: 'plain.txt') fileComment: 'été'.
	bytes := ByteArray streamContents: [ :stream | archive writeTo: stream ].
	self readArchiveFrom: bytes do: [ :readArchive | self assert: readArchive members first fileComment equals: 'été' ]
]

{ #category : 'tests' }
ZipArchiveFileNameEncodingTest >> testNonAsciiNameRoundTrip [

	self
		readArchiveFrom: (self bytesOfArchiveWithMemberNamed: 'café/日本.txt')
		do: [ :archive | self assert: archive members first fileName equals: 'café/日本.txt' ]
]

{ #category : 'tests' }
ZipArchiveFileNameEncodingTest >> testReadsUtf8NameWrittenByPython [

	self readArchiveFrom: self pharoZippedByPython do: [ :archive |
			self assert: archive members first fileName equals: '日本/été.txt'.
			self assert: archive members first contents asString equals: 'Pharo' ]
]

{ #category : 'tests' }
ZipArchiveFileNameEncodingTest >> testUtf8NameWithoutFlagIsReadAsUtf8 [
	"Older versions of Pharo, and many tools, write UTF-8 names without the language encoding flag"

	| bytes |
	bytes := self clearLanguageEncodingFlagIn: (self bytesOfArchiveWithMemberNamed: 'café.txt').
	self deny: ((self flagsIn: bytes) anySatisfy: [ :each | each anyMask: 16r800 ]).
	self readArchiveFrom: bytes do: [ :archive | self assert: archive members first fileName equals: 'café.txt' ]
]
