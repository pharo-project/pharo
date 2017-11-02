"
This is the unit test for the class SecureHashAlgorithm. Unit tests are a good way to exercise the functionality of your system in a repeatable and automatic manner. They are therefore recommended if you plan to release anything. For more information, see: 
	- http://www.c2.com/cgi/wiki?UnitTest
	- there is a chapter in the PharoByExample book (http://pharobyexample.org)
	- the sunit class category
"
Class {
	#name : #SHA1Test,
	#superclass : #ClassTestCase,
	#instVars : [
		'hash'
	],
	#category : 'System-Hashing-Tests-SHA1'
}

{ #category : #'testing - examples' }
SHA1Test >> testEmptyInput [
	"self run: #testEmptyInput"

	self assert: ((SHA1 new hashMessage: '') asInteger radix: 16) equals: 'DA39A3EE5E6B4B0D3255BFEF95601890AFD80709'
]

{ #category : #'testing - examples' }
SHA1Test >> testExample1 [
	"This is the first example from the specification document (FIPS PUB 180-1)"

	hash := SHA1 new hashMessage: 'abc'.
	self assert: hash equals: (ByteArray readHexFrom:  'A9993E364706816ABA3E25717850C26C9CD0D89D').
		
]

{ #category : #'testing - examples' }
SHA1Test >> testExample2 [
	"This is the second example from the specification document (FIPS PUB 180-1)"

	hash := SHA1 new hashMessage: 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq'.
	self assert: hash equals: (ByteArray readHexFrom: '84983E441C3BD26EBAAE4AA1F95129E5E54670F1')
]

{ #category : #'testing - examples' }
SHA1Test >> testExample3 [
	"This is the third example from the specification document (FIPS PUB 180-1). 
	This example may take several minutes."

	hash := SHA1 new hashMessage: (String new: 1000000 withAll: $a).
	self assert: hash asInteger equals: 16r34AA973CD4C4DAA4F61EEB2BDBAD27316534016F.
]

{ #category : #testing }
SHA1Test >> testLargeCharacterStream [
	"Regression test"

	| string |
	string := String streamContents: [ :out | 10 timesRepeat: [ out << Character alphabet ] ].
	hash := SHA1 hashStream: string readStream.
	self assert: hash hex equals: 'f9d5b271f9126e9051394cffaff0ae3250fd6087'
]
