"
I check that GZipReadStream reads all the members of a GZIP file. RFC 1952, section 2.2: ""A gzip file consists of a series of members"", as produced for example by `cat a.gz b.gz > ab.gz`.
"
Class {
	#name : 'GZipMultiMemberTest',
	#superclass : 'TestCase',
	#category : 'Compression-Tests-Streams',
	#package : 'Compression-Tests',
	#tag : 'Streams'
}

{ #category : 'helpers' }
GZipMultiMemberTest >> gzip: aByteArray [

	| writer |
	writer := GZipWriteStream on: ByteArray new.
	writer nextPutAll: aByteArray.
	writer close.
	^ writer encodedStream contents
]

{ #category : 'helpers' }
GZipMultiMemberTest >> pharoGzippedByGzip [
	"Bytes written by
	$ echo -n Pharo | gzip - > /tmp/foo.gz"

	^ #[ 31 139 8 0 94 254 149 93 0 3 11 200 72 44 202 7 0 144 33 178 137 5 0 0 0 ]
]

{ #category : 'helpers' }
GZipMultiMemberTest >> randomBytes: anInteger [
	"Incompressible data, so that the compressed member is about as large as the data"

	| random |
	random := Random seed: 42.
	^ (ByteArray new: anInteger) collect: [ :each | (random nextInteger: 256) - 1 ]
]

{ #category : 'tests' }
GZipMultiMemberTest >> testIgnoresTrailingBytesThatAreNotAMember [
	"Like gzip, ignore trailing garbage such as zero padding"

	| bytes |
	bytes := (self gzip: 'Hello' asByteArray) , #[ 0 0 0 0 ].
	self assert: (GZipReadStream on: bytes) upToEnd asString equals: 'Hello'
]

{ #category : 'tests' }
GZipMultiMemberTest >> testReadsAllMembers [

	| bytes |
	bytes := (self gzip: 'Hello ' asByteArray) , (self gzip: 'World' asByteArray).
	self assert: (GZipReadStream on: bytes) upToEnd asString equals: 'Hello World'.
	self assert: (GZipReadStream on: bytes) contents asString equals: 'Hello World'.
	self assert: bytes unzipped asString equals: 'Hello World'
]

{ #category : 'tests' }
GZipMultiMemberTest >> testReadsAllMembersFromStream [

	| bytes |
	bytes := (self gzip: 'Hello ' asByteArray) , (self gzip: 'World' asByteArray).
	self assert: (GZipReadStream on: bytes readStream) upToEnd asString equals: 'Hello World'
]

{ #category : 'tests' }
GZipMultiMemberTest >> testReadsAllMembersWrittenByGzip [
	"Same as `cat foo.gz foo.gz`"

	self assert: (self pharoGzippedByGzip , self pharoGzippedByGzip) unzipped asString equals: 'PharoPharo'
]

{ #category : 'tests' }
GZipMultiMemberTest >> testReadsEmptyMember [

	| bytes |
	bytes := (self gzip: 'a' asByteArray) , (self gzip: #[  ]) , (self gzip: 'b' asByteArray).
	self assert: (GZipReadStream on: bytes) upToEnd asString equals: 'ab'
]

{ #category : 'tests' }
GZipMultiMemberTest >> testReadsMemberStartingAfterFirstSourceBuffer [
	"When reading from a stream the source is buffered by 64k: the second member must also be found when it starts after the first buffer"

	| data bytes result |
	data := self randomBytes: 100000.
	bytes := (self gzip: data) , (self gzip: 'tail' asByteArray).
	self assert: bytes size > (1 << 16).
	result := (GZipReadStream on: bytes readStream) upToEnd.
	self assert: result size equals: data size + 4.
	self assert: (result first: data size) equals: data.
	self assert: (result last: 4) asString equals: 'tail'
]

{ #category : 'tests' }
GZipMultiMemberTest >> testWrongCrcInSecondMemberRaisesCRCError [

	| second bytes |
	second := self gzip: 'World' asByteArray.
	"The CRC-32 is the 4 bytes before the last 4 bytes (ISIZE)"
	second at: second size - 7 put: ((second at: second size - 7) + 1 bitAnd: 255).
	bytes := (self gzip: 'Hello ' asByteArray) , second.
	self should: [ (GZipReadStream on: bytes) upToEnd ] raise: CRCError
]
