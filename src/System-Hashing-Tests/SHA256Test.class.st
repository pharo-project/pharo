"
SHA256Tests contains unit tests for SHA256.

Test vectors taken from

http://en.wikipedia.org/wiki/Sha256

http://csrc.nist.gov/publications/fips/fips180-2/fips180-2withchangenotice.pdf

"
Class {
	#name : #SHA256Test,
	#superclass : #ClassTestCase,
	#instVars : [
		'hash'
	],
	#category : #'System-Hashing-Tests-SHA256'
}

{ #category : #testing }
SHA256Test >> testFips180Example1 [
	hash := SHA256 hashMessage: 'abc'.
	self assert: hash hex equals: 'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad'
]

{ #category : #testing }
SHA256Test >> testFips180Example2 [
	hash := SHA256 hashMessage: 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq'.
	self assert: hash hex equals: '248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1'
]

{ #category : #testing }
SHA256Test >> testFips180Example3 [
	"This test is a bit slow if the implementation is not making use of extra primitives"
	self timeLimit: 10 seconds.
	
	hash := SHA256 hashMessage: (String new: 1e6 withAll: $a).
	self assert: hash hex equals: 'cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0'
]

{ #category : #testing }
SHA256Test >> testLargeCharacterStream [
	"Regression test"

	| string |
	string := String streamContents: [ :out | 10 timesRepeat: [ out << Character alphabet ] ].
	hash := SHA256 hashStream: string readStream.
	self assert: hash hex equals: '6f6a4460cbd9241d7eff776ce2fd0439e0a865fb05b99f2c5f6f527f226e254d'
]

{ #category : #testing }
SHA256Test >> testWikipediaExample1 [
	hash := SHA256 hashMessage: 'The quick brown fox jumps over the lazy dog'.
	self assert: hash hex equals: 'd7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592'
]

{ #category : #testing }
SHA256Test >> testWikipediaExample2 [
	hash := SHA256 hashMessage: 'The quick brown fox jumps over the lazy dog.'.
	self assert: hash hex equals: 'ef537f25c895bfa782526529a9b63d97aa631564d5d789c2b765448c8635fb6c'
]

{ #category : #testing }
SHA256Test >> testWikipediaExampleEmpty [
	hash := SHA256 hashMessage: ''.
	self assert: hash hex equals: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'
]
