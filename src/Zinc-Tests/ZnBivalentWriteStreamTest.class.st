Class {
	#name : #ZnBivalentWriteStreamTest,
	#superclass : #TestCase,
	#category : #'Zinc-Tests'
}

{ #category : #testing }
ZnBivalentWriteStreamTest >> testByteWriting [
	| string |
	string := String streamContents: [ :stream | | writeStream |
		writeStream := ZnBivalentWriteStream on: stream.
		writeStream nextPut: 97; nextPutAll: #(98 99) asByteArray ].
	self assert: string asByteArray = #(97 98 99) asByteArray
]

{ #category : #testing }
ZnBivalentWriteStreamTest >> testCharacterWriting [
	| bytes |
	bytes := ByteArray streamContents: [ :stream | | writeStream |
		writeStream := ZnBivalentWriteStream on: stream.
		writeStream nextPut: $a; space; nextPutAll: '123' ].
	self assert: bytes = 'a 123' asByteArray
]
