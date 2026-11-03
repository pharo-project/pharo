"
I check that GZipReadStream handles the header flags of RFC 1952, section 2.3.1: FTEXT, FHCRC, FEXTRA, FNAME and FCOMMENT, in this order in the header, and rejects the reserved bits 5 to 7.

I build the headers myself and reuse the compressed data and trailer written by GZipWriteStream.
"
Class {
	#name : 'GZipHeaderFlagsTest',
	#superclass : 'TestCase',
	#category : 'Compression-Tests-Streams',
	#package : 'Compression-Tests',
	#tag : 'Streams'
}

{ #category : 'helpers' }
GZipHeaderFlagsTest >> compressedDataAndTrailerOf: aString [
	"Answer what GZipWriteStream writes after its 10 bytes header: the compressed data, CRC-32 and ISIZE"

	| writer |
	writer := GZipWriteStream on: ByteArray new.
	writer nextPutAll: aString asByteArray.
	writer close.
	^ writer encodedStream contents allButFirst: 10
]

{ #category : 'helpers' }
GZipHeaderFlagsTest >> member: aString flags: flags fields: fieldBytes [
	"Answer a GZIP member with the given flags, optional fields (FEXTRA, FNAME, FCOMMENT) and, when FHCRC is set, the header CRC"

	| header crc |
	header := #[ 16r1F 16r8B 8 ] , { flags } asByteArray , #[ 0 0 0 0 0 3 ] , fieldBytes.
	(flags anyMask: 2) ifTrue: [
			crc := CRC crc32FromCollection: header.
			header := header , {
				          (crc bitAnd: 16rFF).
				          ((crc bitShift: -8) bitAnd: 16rFF) } asByteArray ].
	^ header , (self compressedDataAndTrailerOf: aString)
]

{ #category : 'tests' }
GZipHeaderFlagsTest >> testFextraIsSkipped [

	| member |
	member := self member: 'Hello' flags: 16r04 fields: #[ 3 0 65 66 67 ].
	self assert: (GZipReadStream on: member) upToEnd asString equals: 'Hello'
]

{ #category : 'tests' }
GZipHeaderFlagsTest >> testFhcrcAlone [

	self assert: (GZipReadStream on: (self member: 'Hello' flags: 16r02 fields: #[  ])) upToEnd asString equals: 'Hello'
]

{ #category : 'tests' }
GZipHeaderFlagsTest >> testFhcrcComesAfterTheOtherFields [
	"The header CRC is the last field of the header, after FEXTRA, FNAME and FCOMMENT"

	| member |
	member := self
		          member: 'Hello'
		          flags: 16r02 | 16r04 | 16r08 | 16r10
		          fields: #[ 2 0 1 2 ] , (self zeroTerminated: 'hello.txt') , (self zeroTerminated: 'a comment').
	self assert: (GZipReadStream on: member) upToEnd asString equals: 'Hello'
]

{ #category : 'tests' }
GZipHeaderFlagsTest >> testFhcrcInSecondMember [

	| bytes |
	bytes := (self member: 'Hello ' flags: 0 fields: #[  ])
	         , (self member: 'World' flags: 16r02 | 16r08 fields: (self zeroTerminated: 'world.txt')).
	self assert: (GZipReadStream on: bytes) upToEnd asString equals: 'Hello World'
]

{ #category : 'tests' }
GZipHeaderFlagsTest >> testFnameAndFcommentAreSkipped [

	| member |
	member := self
		          member: 'Hello'
		          flags: 16r08 | 16r10
		          fields: (self zeroTerminated: 'hello.txt') , (self zeroTerminated: 'a comment').
	self assert: (GZipReadStream on: member) upToEnd asString equals: 'Hello'
]

{ #category : 'tests' }
GZipHeaderFlagsTest >> testFtextIsAccepted [

	self assert: (GZipReadStream on: (self member: 'Hello' flags: 16r01 fields: #[  ])) upToEnd asString equals: 'Hello'
]

{ #category : 'tests' }
GZipHeaderFlagsTest >> testNoFlags [

	self assert: (GZipReadStream on: (self member: 'Hello' flags: 0 fields: #[  ])) upToEnd asString equals: 'Hello'
]

{ #category : 'tests' }
GZipHeaderFlagsTest >> testReservedFlagsAreRejected [

	#( 16r20 16r40 16r80 ) do: [ :each |
		self should: [ GZipReadStream on: (self member: 'Hello' flags: each fields: #[  ]) ] raise: Error ]
]

{ #category : 'tests' }
GZipHeaderFlagsTest >> testWrongFhcrcRaisesCRCError [

	| member crcIndex |
	member := self member: 'Hello' flags: 16r02 fields: #[  ].
	crcIndex := 11.
	member at: crcIndex put: ((member at: crcIndex) + 1 bitAnd: 255).
	self should: [ (GZipReadStream on: member) upToEnd ] raise: CRCError
]

{ #category : 'helpers' }
GZipHeaderFlagsTest >> zeroTerminated: aString [

	^ aString asByteArray , #[ 0 ]
]
