Class {
	#name : #ZnBivalentWriteStreamTest,
	#superclass : #TestCase,
	#instVars : [
		'bivalentWriteStream'
	],
	#category : #'Zinc-Tests'
}

{ #category : #running }
ZnBivalentWriteStreamTest >> setUp [ 
	super setUp.
	bivalentWriteStream := ZnBivalentWriteStream on: String new writeStream
]

{ #category : #running }
ZnBivalentWriteStreamTest >> tearDown [ 

	bivalentWriteStream close.
	super tearDown.
]

{ #category : #testing }
ZnBivalentWriteStreamTest >> testAlwaysAtEnd [

	self assert: bivalentWriteStream atEnd.
	
	bivalentWriteStream nextPutAll: 'move'.
	
	self assert: bivalentWriteStream atEnd
	
]

{ #category : #testing }
ZnBivalentWriteStreamTest >> testByteWriting [
	| string |
	string := String
		streamContents: [ :stream | 
			| writeStream |
			writeStream := ZnBivalentWriteStream on: stream.
			writeStream
				nextPut: 97;
				nextPutAll: #(98 99) asByteArray ].
	self assert: string asByteArray equals: #(97 98 99) asByteArray
]

{ #category : #testing }
ZnBivalentWriteStreamTest >> testCharacterWriting [
	| bytes |
	bytes := ByteArray
		streamContents: [ :stream | 
			| writeStream |
			writeStream := ZnBivalentWriteStream on: stream.
			writeStream
				nextPut: $a;
				space;
				nextPutAll: '123' ].
	self assert: bytes equals: 'a 123' asByteArray
]

{ #category : #testing }
ZnBivalentWriteStreamTest >> testRetrieveContents [

	self assert: bivalentWriteStream contents isEmpty.
	
	bivalentWriteStream nextPutAll: 'something'.
	
	self assert: bivalentWriteStream contents equals: 'something'
	
]
